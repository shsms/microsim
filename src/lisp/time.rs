use std::{fmt::Display, ops::Deref};

use tulisp::{Error, Shared, TulispContext, TulispConvertible, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("dt:now", || TulispDateTime::now());
    ctx.defun("dt:minutes", |minutes: i64| {
        TulispTimeDelta::from(chrono::Duration::minutes(minutes))
    });
    ctx.defun("dt:milliseconds", |milliseconds: i64| {
        TulispTimeDelta::from(chrono::Duration::milliseconds(milliseconds))
    });

    ctx.defun(
        "dt+",
        |a: DateTimeTimeDelta, b: DateTimeTimeDelta| -> Result<TulispObject, Error> {
            match (a, b) {
                (DateTimeTimeDelta::DateTime(dt), DateTimeTimeDelta::TimeDelta(td))
                | (DateTimeTimeDelta::TimeDelta(td), DateTimeTimeDelta::DateTime(dt)) => {
                    Ok(TulispDateTime(dt.0 + td.0).into_tulisp())
                }
                _ => Err(Error::type_mismatch(
                    "dt+: Expected TulispDateTime + TulispTimeDelta".to_string(),
                )),
            }
        },
    );

    ctx.defun(
            "dt-",
            |a: DateTimeTimeDelta, b: DateTimeTimeDelta| -> Result<TulispObject, Error> {
                match (a, b) {
                    (DateTimeTimeDelta::DateTime(dt), DateTimeTimeDelta::TimeDelta(td)) => {
                        Ok(TulispDateTime(dt.0 - td.0).into_tulisp())
                }
                    (DateTimeTimeDelta::DateTime(dt1), DateTimeTimeDelta::DateTime(dt2)) => {
                        Ok(TulispTimeDelta(dt1.0 - dt2.0).into_tulisp())
                }
                    _ => Err(Error::type_mismatch(
                        "dt-: Expected TulispDateTime - TulispTimeDelta or TulispDateTime - TulispDateTime"
                            .to_string(),
                )),
            }
        },
    );

    ctx.defun(
        "dt:format",
        |timestamp: TulispDateTime, format: Option<String>| -> Result<String, Error> {
            Ok(timestamp
                .0
                .format(format.as_deref().unwrap_or("%Y-%m-%d %H:%M:%S%.f %:z"))
                .to_string())
        },
    );

    ctx.defun("dt:dur->milliseconds", |delta: TulispTimeDelta| -> i64 {
        delta.0.num_milliseconds()
    });

    ctx.defun(
        "dt:epoch-align",
        |timestamp: TulispDateTime, interval: TulispTimeDelta| -> TulispObject {
            epoch_align(timestamp.0, interval.0)
                .map(|dt| TulispDateTime(dt).into_tulisp())
                .unwrap_or_else(|| false.into())
        },
    );
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct TulispDateTime(chrono::DateTime<chrono::Utc>);

impl Display for TulispDateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<dt:{}>", self.0.to_rfc3339())
    }
}

impl TulispConvertible for TulispDateTime {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<TulispDateTime>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch("Expected TulispDateTime".to_string())),
            },
            Err(_) => Err(Error::type_mismatch("Expected TulispDateTime".to_string())),
        }
    }

    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
    }
}

impl From<chrono::DateTime<chrono::Utc>> for TulispDateTime {
    fn from(value: chrono::DateTime<chrono::Utc>) -> Self {
        TulispDateTime(value)
    }
}

impl Deref for TulispDateTime {
    type Target = chrono::DateTime<chrono::Utc>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<TulispObject> for TulispDateTime {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<TulispDateTime>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch("Expected TulispDateTime".to_string())),
            },
            Err(_) => Err(Error::type_mismatch("Expected TulispDateTime".to_string())),
        }
    }
}

impl TulispDateTime {
    pub fn now() -> Self {
        TulispDateTime(chrono::Utc::now())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TulispTimeDelta(chrono::TimeDelta);

impl Display for TulispTimeDelta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<td:{}ms>", self.0.num_milliseconds())
    }
}

impl From<chrono::TimeDelta> for TulispTimeDelta {
    fn from(value: chrono::TimeDelta) -> Self {
        TulispTimeDelta(value)
    }
}

impl TulispConvertible for TulispTimeDelta {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<TulispTimeDelta>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch(format!(
                    "Expected TulispTimeDelta, got: {value}"
                ))),
            },
            Err(_) => Err(Error::type_mismatch(format!(
                "Expected TulispTimeDelta, got: {value}"
            ))),
        }
    }

    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
    }
}

#[derive(Debug, Clone)]
enum DateTimeTimeDelta {
    DateTime(TulispDateTime),
    TimeDelta(TulispTimeDelta),
}

impl TulispConvertible for DateTimeTimeDelta {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if let Ok(dt) = TulispConvertible::from_tulisp(&value) {
            Ok(DateTimeTimeDelta::DateTime(dt))
        } else if let Ok(td) = TulispConvertible::from_tulisp(&value) {
            Ok(DateTimeTimeDelta::TimeDelta(td))
        } else {
            Err(Error::type_mismatch(format!(
                "Expected TulispDateTime or TulispTimeDelta, got: {value}"
            )))
        }
    }

    fn into_tulisp(self) -> TulispObject {
        match self {
            DateTimeTimeDelta::DateTime(dt) => dt.into_tulisp(),
            DateTimeTimeDelta::TimeDelta(td) => td.into_tulisp(),
        }
    }
}

fn epoch_align(
    timestamp: chrono::DateTime<chrono::Utc>,
    interval: chrono::TimeDelta,
) -> Option<chrono::DateTime<chrono::Utc>> {
    let millis_since_epoch = timestamp.timestamp_millis();
    let interval_millis = interval.num_milliseconds();

    let intervals_since_epoch = millis_since_epoch / interval_millis;
    let aligned_millis_since_epoch = intervals_since_epoch * interval_millis;

    let aligned_timestamp = chrono::DateTime::from_timestamp_millis(aligned_millis_since_epoch)?;

    Some(aligned_timestamp)
}
