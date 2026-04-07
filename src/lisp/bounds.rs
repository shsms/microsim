mod bounds_methods;

mod vec_bounds;
pub(crate) use vec_bounds::VecBounds;

use std::collections::VecDeque;

use chrono::Duration;
use tulisp::{Error, Rest, Shared, TulispConvertible, TulispObject};

use crate::{lisp::time::TulispDateTime, proto::common::metrics::Bounds};

pub(crate) fn add(ctx: &mut tulisp::TulispContext) {
    ctx.add_function(
        "bounds/add",
        |mut bounds: TulispComponentBounds,
         create_ts: TulispDateTime,
         new_bounds: VecBounds,
         lifetime_s: i64| {
            bounds
                .augmented
                .push_back((create_ts, new_bounds, lifetime_s));
            bounds
        },
    );

    ctx.add_function(
        "bounds/add-raw",
        |mut bounds: TulispComponentBounds,
         create_ts: TulispDateTime,
         lower: f64,
         upper: f64,
         lifetime_s: i64| {
            bounds.augmented.push_back((
                create_ts,
                VecBounds::new(vec![Bounds {
                    lower: Some(lower as f32),
                    upper: Some(upper as f32),
                }]),
                lifetime_s,
            ));
            bounds
        },
    );

    ctx.add_function(
        "bounds/make-container",
        |rated_lower: f64, rated_upper: f64| {
            TulispComponentBounds::new(VecBounds::new(vec![Bounds {
                lower: Some(rated_lower as f32),
                upper: Some(rated_upper as f32),
            }]))
        },
    );

    ctx.add_function(
        "bounds/drop-expired",
        |mut bounds: TulispComponentBounds| -> TulispComponentBounds {
            let now = TulispDateTime::now();
            while let Some((ts, _, dur)) = bounds.augmented.front() {
                if **ts + Duration::seconds(*dur) < *now {
                    bounds.augmented.pop_front();
                } else {
                    break;
                }
            }
            bounds
        },
    );

    ctx.add_function(
        "bounds/contains",
        |bounds: TulispComponentBounds, value: f64| -> bool {
            bounds.squash().contains(value as f32)
        },
    );

    ctx.add_function(
        "bounds/contains-in-sum",
        |value: f64, bounds_list: Rest<TulispComponentBounds>| -> bool {
            let total_bounds = bounds_list
                .into_iter()
                .fold(VecBounds(vec![]), |acc, b| acc.add(&b.squash()));
            total_bounds.contains(value as f32)
        },
    );

    ctx.add_function(
        "bounds/limit-power",
        |bounds: TulispComponentBounds, measured_power: f64| -> f64 {
            bounds.squash().limit_power(measured_power)
        },
    );
}

#[derive(Debug, Clone)]
pub(crate) struct TulispComponentBounds {
    pub(crate) rated_bounds: VecBounds,
    pub(crate) augmented: VecDeque<(TulispDateTime, VecBounds, i64)>,
}

impl std::fmt::Display for TulispComponentBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#<rated-bounds: {}, component-bounds: {:?}>",
            self.rated_bounds, self.augmented
        )
    }
}

impl TulispConvertible for TulispComponentBounds {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<TulispComponentBounds>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch(format!(
                    "Expected TulispComponentBounds, got {value}."
                ))),
            },
            Err(_) => Err(Error::type_mismatch(format!(
                "Expected TulispComponentBounds, got {value}."
            ))),
        }
    }

    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
    }
}

impl TulispComponentBounds {
    pub fn new(rated_bounds: VecBounds) -> Self {
        TulispComponentBounds {
            rated_bounds,
            augmented: VecDeque::new(),
        }
    }

    pub fn squash(&self) -> VecBounds {
        let mut bounds = self.rated_bounds.clone();

        for (_, b, _) in self.augmented.iter() {
            bounds = bounds.intersect(b);
        }
        return bounds;
    }
}
