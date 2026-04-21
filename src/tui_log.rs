use std::{
    collections::VecDeque,
    io::Write,
    sync::{Arc, Mutex},
};

use log::{LevelFilter, Log, Metadata, Record, SetLoggerError};

#[derive(Clone)]
pub struct LogBuffer {
    inner: Arc<Mutex<VecDeque<String>>>,
    cap: usize,
}

impl LogBuffer {
    pub fn new(cap: usize) -> Self {
        Self {
            inner: Arc::new(Mutex::new(VecDeque::with_capacity(cap))),
            cap,
        }
    }

    pub fn lines(&self) -> Vec<String> {
        self.inner.lock().unwrap().iter().cloned().collect()
    }

    fn push(&self, line: String) {
        let mut buf = self.inner.lock().unwrap();
        while buf.len() >= self.cap {
            buf.pop_front();
        }
        buf.push_back(line);
    }
}

struct TuiLogger {
    buffer: LogBuffer,
    level: LevelFilter,
    file: Option<Mutex<std::fs::File>>,
}

impl Log for TuiLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= self.level
    }

    fn log(&self, record: &Record) {
        if !self.enabled(record.metadata()) {
            return;
        }
        let line = format!(
            "{} [{}] {}",
            chrono::Local::now().format("%H:%M:%S%.3f"),
            record.level(),
            record.args()
        );
        self.buffer.push(line.clone());
        if let Some(file) = &self.file {
            let mut f = file.lock().unwrap();
            let _ = writeln!(f, "{line}");
        }
    }

    fn flush(&self) {
        if let Some(file) = &self.file {
            let _ = file.lock().unwrap().flush();
        }
    }
}

pub fn install(
    buffer: LogBuffer,
    level: LevelFilter,
    tee_file: Option<std::fs::File>,
) -> Result<(), SetLoggerError> {
    let logger = TuiLogger {
        buffer,
        level,
        file: tee_file.map(Mutex::new),
    };
    log::set_max_level(level);
    log::set_boxed_logger(Box::new(logger))
}
