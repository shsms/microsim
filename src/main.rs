mod lisp;
mod proto;
mod server;
mod timeout_tracker;

use clap::Parser;
use proto::microgrid::v1alpha18::microgrid_server;
use tonic::transport::Server;

#[derive(Parser)]
#[command(name = "microsim")]
#[command(about = "Microgrid simulator", long_about = None)]
struct Args {
    /// Path to the configuration file
    #[arg(short, long, default_value = "config.lisp")]
    config: String,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    simplelog::SimpleLogger::init(simplelog::LevelFilter::Debug, simplelog::Config::default())
        .unwrap();

    let args = Args::parse();
    let config = lisp::Config::new(&args.config);
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
