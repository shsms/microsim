use std::{fmt::Display, ops::Deref};

use tulisp::{Error, Shared, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_function("dt:now", || TulispDateTime::now());
    ctx.add_function("dt:minutes", |minutes: i64| {
        TulispTimeDelta::from(chrono::Duration::minutes(minutes))
    });
    ctx.add_function("dt:milliseconds", |milliseconds: i64| {
        TulispTimeDelta::from(chrono::Duration::milliseconds(milliseconds))
    });

    ctx.add_function(
        "dt+",
        |a: DateTimeTimeDelta, b: DateTimeTimeDelta| -> Result<TulispObject, Error> {
            match (a, b) {
                (DateTimeTimeDelta::DateTime(dt), DateTimeTimeDelta::TimeDelta(td))
                | (DateTimeTimeDelta::TimeDelta(td), DateTimeTimeDelta::DateTime(dt)) => {
                    Ok(TulispDateTime(dt.0 + td.0).into())
                }
                _ => Err(Error::type_mismatch(
                    "dt+: Expected TulispDateTime + TulispTimeDelta".to_string(),
                )),
            }
        },
    );

    ctx.add_function(
        "dt-",
        |a: DateTimeTimeDelta, b: DateTimeTimeDelta| -> Result<TulispObject, Error> {
            match (a, b) {
                (DateTimeTimeDelta::DateTime(dt), DateTimeTimeDelta::TimeDelta(td)) => {
                    Ok(TulispDateTime(dt.0 - td.0).into())
                }
                (DateTimeTimeDelta::DateTime(dt1), DateTimeTimeDelta::DateTime(dt2)) => {
                    Ok(TulispTimeDelta(dt1.0 - dt2.0).into())
                }
                _ => Err(Error::type_mismatch(
                    "dt-: Expected TulispDateTime - TulispTimeDelta or TulispDateTime - TulispDateTime"
                        .to_string(),
                )),
            }
        },
    );

    ctx.add_function(
        "dt:format",
        |timestamp: TulispDateTime, format: Option<String>| -> Result<String, Error> {
            Ok(timestamp
                .0
                .format(format.as_deref().unwrap_or("%Y-%m-%d %H:%M:%S%.f %:z"))
                .to_string())
        },
    );

    ctx.add_function("dt:dur->milliseconds", |delta: TulispTimeDelta| -> i64 {
        delta.0.num_milliseconds()
    });

    ctx.add_function(
        "dt:epoch-align",
        |timestamp: TulispDateTime, interval: TulispTimeDelta| -> TulispObject {
            epoch_align(timestamp.0, interval.0)
                .map(|dt| TulispDateTime(dt).into())
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

impl From<chrono::DateTime<chrono::Utc>> for TulispDateTime {
    fn from(value: chrono::DateTime<chrono::Utc>) -> Self {
        TulispDateTime(value)
    }
}

impl From<TulispDateTime> for TulispObject {
    fn from(value: TulispDateTime) -> Self {
        Shared::new(value).into()
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

impl From<TulispTimeDelta> for TulispObject {
    fn from(value: TulispTimeDelta) -> Self {
        Shared::new(value).into()
    }
}

impl TryFrom<TulispObject> for TulispTimeDelta {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<TulispTimeDelta>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch("Expected TulispTimeDelta".to_string())),
            },
            Err(_) => Err(Error::type_mismatch("Expected TulispTimeDelta".to_string())),
        }
    }
}

#[derive(Debug, Clone)]
enum DateTimeTimeDelta {
    DateTime(TulispDateTime),
    TimeDelta(TulispTimeDelta),
}

impl TryFrom<TulispObject> for DateTimeTimeDelta {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        if let Ok(dt) = TulispDateTime::try_from(value.clone()) {
            Ok(DateTimeTimeDelta::DateTime(dt))
        } else if let Ok(td) = TulispTimeDelta::try_from(value.clone()) {
            Ok(DateTimeTimeDelta::TimeDelta(td))
        } else {
            Err(Error::type_mismatch(
                "Expected TulispDateTime or TulispTimeDelta".to_string(),
            ))
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
