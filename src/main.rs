mod lisp;
mod proto;
mod server;
mod timeout_tracker;

use proto::microgrid::v1alpha18::microgrid_server;
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

    let load_path = if first_arg == "--help" {
            println!("
                Usage: microsim [--help] [CONFIG_PATH]

                Options:
                  --help  Show this help and exit

                Args:
                  CONFIG_PATH  Path to the config file
            ");
            return;
        } else {
            first_arg
    };


    let config = lisp::Config::new(&load_path);
    tokio::spawn(config.clone().start());
    let socket_addr = config.socket_addr();
    log::info!("Server listening on {}", socket_addr);

    let server = server::MicrogridServer::new(config);
    Server::builder()
        .add_service(microgrid_server::MicrogridServer::new(server))
        .serve(socket_addr.parse().unwrap())
        .await
        .unwrap();
}
