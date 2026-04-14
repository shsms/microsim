use std::{env, io::Write as _, path::Path};

#[tokio::main(flavor = "current_thread")]
async fn main() {
    simplelog::SimpleLogger::init(simplelog::LevelFilter::Debug, simplelog::Config::default())
        .unwrap();

    let mut args = env::args();
    let _program_path = args.next();

    // If no argument provided, the "./config.lisp" is used as default
    let base_path = args.next().unwrap();
    let base_path = Path::new(&base_path);

    let load_path = base_path.join("config.lisp").to_str().unwrap().to_string();

    let mut next = args.next();

    while let Some("-") = next.as_deref() {
        next = args.next();
    }

    let output_path = if let Some("-o") = next.as_deref() {
        args.next().unwrap_or_else(|| {
            log::error!("Output path not provided after -o");
            std::process::exit(1);
        })
    } else {
        "TAGS".to_string()
    };

    let config = microsim::lisp::Config::tags_table(&load_path);

    std::fs::File::create(&output_path)
        .unwrap()
        .write(config.unwrap().as_bytes())
        .unwrap();
    log::info!("Updated TAGS file: {}", output_path);
}
