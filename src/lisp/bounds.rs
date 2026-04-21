mod bounds_methods;

mod tulisp_components_bounds;
pub(crate) use tulisp_components_bounds::TulispComponentBounds;

mod vec_bounds;
pub(crate) use vec_bounds::VecBounds;

pub(crate) fn add(ctx: &mut tulisp::TulispContext) {
    use crate::{lisp::time::TulispDateTime, proto::common::metrics::Bounds};

    ctx.defun(
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

    ctx.defun(
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

    ctx.defun(
        "bounds/make-container",
        |rated_lower: f64, rated_upper: f64| {
            TulispComponentBounds::new(VecBounds::new(vec![Bounds {
                lower: Some(rated_lower as f32),
                upper: Some(rated_upper as f32),
            }]))
        },
    );

    ctx.defun(
        "bounds/drop-expired",
        |mut bounds: TulispComponentBounds| -> TulispComponentBounds {
            let now = TulispDateTime::now();
            while let Some((ts, _, dur)) = bounds.augmented.front() {
                if **ts + chrono::Duration::seconds(*dur) < *now {
                    bounds.augmented.pop_front();
                } else {
                    break;
                }
            }
            bounds
        },
    );

    ctx.defun(
        "bounds/contains",
        |bounds: TulispComponentBounds, value: f64| -> bool {
            bounds.squash().contains(value as f32)
        },
    );

    ctx.defun(
        "bounds/contains-in-sum",
        |value: f64, bounds_list: tulisp::Rest<TulispComponentBounds>| -> bool {
            let total_bounds = bounds_list
                .into_iter()
                .fold(VecBounds(vec![]), |acc, b| acc.add(&b.squash()));
            total_bounds.contains(value as f32)
        },
    );

    ctx.defun(
        "bounds/limit-power",
        |bounds: TulispComponentBounds, measured_power: f64| -> f64 {
            bounds.squash().limit_power(measured_power)
        },
    );
}
