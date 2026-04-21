use microsim::proto::microgrid::microgrid_server;
use std::env;
use tonic::transport::Server;

const DEFAULT_CONFIG_PATH: &str = "./config.lisp";
const DEFAULT_TUI_LOG_PATH: &str = "/tmp/microsim-tui.log";

const HELP: &str = r#"Usage: microsim [--help] [--tui] [--log-file PATH] [CONFIG_PATH]

Options:
  --help            Show this help and exit
  --tui             Run the interactive TUI from sim/tui.lisp (loaded by config.lisp)
  --log-file PATH   Write logs to PATH (truncated on each run) instead of stderr.
                    Implied by --tui (defaults to /tmp/microsim-tui.log).

Args:
  CONFIG_PATH       Path to the config file (default: ./config.lisp)
"#;

struct Args {
    tui: bool,
    log_file: Option<String>,
    config_path: String,
}

fn parse_args() -> Args {
    let mut tui = false;
    let mut log_file: Option<String> = None;
    let mut config_path: Option<String> = None;
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" => {
                print!("{HELP}");
                std::process::exit(0);
            }
            "--tui" => tui = true,
            "--log-file" => {
                log_file = Some(args.next().unwrap_or_else(|| {
                    eprintln!("--log-file requires a PATH argument");
                    std::process::exit(2);
                }));
            }
            s if let Some(v) = s.strip_prefix("--log-file=") => {
                log_file = Some(v.to_string());
            }
            other => config_path = Some(other.to_string()),
        }
    }
    Args {
        tui,
        log_file,
        config_path: config_path.unwrap_or_else(|| DEFAULT_CONFIG_PATH.to_string()),
    }
}

/// Install a logger appropriate for how microsim was invoked. TUI runs route
/// logs into an in-memory ring buffer (the log pane reads from it); non-TUI
/// runs log directly to a file or stderr.
fn init_logging(tui: bool, log_file: Option<&str>) -> Option<microsim::tui_log::LogBuffer> {
    let tee_file = log_file.map(|path| {
        std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)
            .unwrap_or_else(|e| panic!("open {}: {}", path, e))
    });

    if tui {
        let buffer = microsim::tui_log::LogBuffer::new(500);
        microsim::tui_log::install(buffer.clone(), log::LevelFilter::Debug, tee_file)
            .expect("install tui logger");
        Some(buffer)
    } else {
        let cfg = simplelog::Config::default();
        match tee_file {
            Some(f) => simplelog::WriteLogger::init(simplelog::LevelFilter::Debug, cfg, f).unwrap(),
            None => simplelog::SimpleLogger::init(simplelog::LevelFilter::Debug, cfg).unwrap(),
        }
        None
    }
}

async fn serve_grpc(server: microsim::server::MicrogridServer, addr: String) {
    Server::builder()
        .add_service(microgrid_server::MicrogridServer::new(server))
        .serve(addr.parse().unwrap())
        .await
        .unwrap();
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let args = parse_args();

    // TUI takes over the terminal, so stdout/stderr logging would scribble
    // over the UI — fall back to a default log path if the user didn't pick one.
    let log_file = args
        .log_file
        .or_else(|| args.tui.then(|| DEFAULT_TUI_LOG_PATH.to_string()));
    let log_buffer = init_logging(args.tui, log_file.as_deref());

    let config = microsim::lisp::Config::new(&args.config_path);
    if let Some(buffer) = log_buffer {
        config.register_log_buffer(buffer);
    }
    tokio::spawn(config.clone().start());

    let socket_addr = config.socket_addr();
    let server = microsim::server::MicrogridServer::new(config.clone());

    if args.tui {
        tokio::spawn(serve_grpc(server, socket_addr));
        if config.run_tui().await.is_err() {
            match &log_file {
                Some(p) => eprintln!("tui exited with error; see {p}"),
                None => eprintln!("tui exited with error"),
            }
        }
    } else {
        log::info!("Server listening on {}", socket_addr);
        serve_grpc(server, socket_addr).await;
    }
}
