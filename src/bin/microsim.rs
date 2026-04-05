use microsim::proto::microgrid::v1alpha18::microgrid_server;
use std::env;
use tonic::transport::Server;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    simplelog::SimpleLogger::init(simplelog::LevelFilter::Debug, simplelog::Config::default())
        .unwrap();

    let mut args = env::args();
    let _program_path = args.next();

    // If no argument provided, the "./config.lisp" is used as default
    let first_arg = args.next().unwrap_or("./config.lisp".to_string());

    if first_arg == "--help" {
        println!(
            r#"Usage: microsim [--help] [CONFIG_PATH]

Options:
  --help  Show this help and exit

Args:
  CONFIG_PATH  Path to the config file
"#
        );
        return;
    }
    let load_path = first_arg;

    let config = microsim::lisp::Config::new(&load_path);
    tokio::spawn(config.clone().start());
    let socket_addr = config.socket_addr();
    log::info!("Server listening on {}", socket_addr);

    let server = microsim::server::MicrogridServer::new(config);
    Server::builder()
        .add_service(microgrid_server::MicrogridServer::new(server))
        .serve(socket_addr.parse().unwrap())
        .await
        .unwrap();
}
