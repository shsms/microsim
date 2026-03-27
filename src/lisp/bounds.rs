use std::collections::VecDeque;

use chrono::Duration;
use tulisp::{Error, Rest, Shared, TulispObject};

use crate::{lisp::time::TulispDateTime, proto::common::v1alpha8::metrics::Bounds};

pub(crate) fn add(ctx: &mut tulisp::TulispContext) {
    ctx.add_function(
        "bounds/add",
        |mut bounds: TulispComponentBounds, create_ts: TulispDateTime, new_bounds: VecBounds| {
            bounds.augmented.push_back((create_ts, new_bounds));
            bounds
        },
    );

    ctx.add_function(
        "bounds/add-raw",
        |mut bounds: TulispComponentBounds,
         create_ts: TulispDateTime,
         lower: Option<f64>,
         upper: Option<f64>| {
            bounds.augmented.push_back((
                create_ts,
                VecBounds::new(vec![Bounds {
                    lower: lower.map(|x| x as f32),
                    upper: upper.map(|x| x as f32),
                }]),
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
            while let Some((ts, _)) = bounds.augmented.front() {
                if **ts + Duration::seconds(5) < *now {
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
    pub(crate) augmented: VecDeque<(TulispDateTime, VecBounds)>,
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

impl From<TulispComponentBounds> for TulispObject {
    fn from(value: TulispComponentBounds) -> Self {
        Shared::new(value).into()
    }
}

impl TryFrom<TulispObject> for TulispComponentBounds {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<TulispComponentBounds>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch("Expected ActiveVecBounds".to_string())),
            },
            Err(_) => Err(Error::type_mismatch("Expected ActiveVecBounds".to_string())),
        }
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

        for (_, b) in self.augmented.iter() {
            bounds = bounds.intersect(b);
        }
        return bounds;
    }
}

#[derive(Debug, Clone)]
pub(crate) struct VecBounds(pub(crate) Vec<Bounds>);

impl std::fmt::Display for VecBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<bounds: {:?}>", self.0)
    }
}

impl From<VecBounds> for TulispObject {
    fn from(value: VecBounds) -> Self {
        Shared::new(value).into()
    }
}

impl TryFrom<TulispObject> for VecBounds {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<VecBounds>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch("Expected AugmentedBounds".to_string())),
            },
            Err(_) => Err(Error::type_mismatch("Expected AugmentedBounds".to_string())),
        }
    }
}

impl VecBounds {
    pub fn new(mut bounds: Vec<Bounds>) -> Self {
        bounds.sort_by(|a, b| {
            let a_lower = a.lower.unwrap_or(f32::MIN);
            let b_lower = b.lower.unwrap_or(f32::MIN);
            a_lower
                .partial_cmp(&b_lower)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        VecBounds(bounds)
    }

    pub fn contains(&self, value: f32) -> bool {
        self.0.iter().any(|b| b.contains(value))
    }

    pub fn intersect(&self, other: &Self) -> Self {
        let mut result = Vec::new();
        for b1 in &self.0 {
            for b2 in &other.0 {
                let int = b1.intersect(b2);
                if int.lower.is_some() || int.upper.is_some() {
                    result.push(int);
                }
            }
        }
        Self(result)
    }

    pub fn add(&self, other: &Self) -> Self {
        match (self.0.as_slice(), &other.0.as_slice()) {
            ([a], []) | ([], [a]) => Self(vec![a.clone()]),
            ([a], [b]) => Self(vec![a.add(b)]),
            ([a_first, .., a_last], [b_first, .., b_last]) => {
                Self(vec![a_first.add(b_first), a_last.add(b_last)])
            }
            _ => Self(vec![]), // TODO: Handle more complex cases if needed
        }
    }

    pub fn limit_power(&self, measured_power: f64) -> f64 {
        let limited_power = measured_power;
        let mut prev_bounds = None;

        for bounds in &self.0 {
            if bounds.contains(measured_power as f32) {
                return measured_power;
            }

            if measured_power < *bounds {
                match (prev_bounds, *bounds) {
                    (
                        Some(Bounds {
                            upper: Some(prev_upper),
                            ..
                        }),
                        Bounds {
                            lower: Some(lower), ..
                        },
                    ) => {
                        if (measured_power - prev_upper as f64).abs()
                            < (lower as f64 - measured_power).abs()
                        {
                            return prev_upper as f64;
                        } else {
                            return lower as f64;
                        }
                    }
                    _ => {
                        return bounds.lower.map(|x| x as f64).unwrap_or(measured_power);
                    }
                }
            }

            prev_bounds = Some(*bounds);
        }
        if let Some(Bounds {
            upper: Some(upper), ..
        }) = prev_bounds
        {
            return upper as f64;
        } else if let Some(Bounds {
            lower: Some(lower), ..
        }) = prev_bounds
        {
            return lower as f64;
        }
        limited_power
    }
}

impl Bounds {
    fn any_or(f: impl FnOnce(f32, f32) -> f32, a: Option<f32>, b: Option<f32>) -> Option<f32> {
        match (a, b) {
            (Some(a), Some(b)) => Some(f(a, b)),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        }
    }

    pub fn intersect(&self, other: &Self) -> Self {
        let lower = Self::any_or(f32::max, self.lower, other.lower);
        let upper = Self::any_or(f32::min, self.upper, other.upper);
        if let (Some(lower), Some(upper)) = (lower, upper) {
            if lower > upper {
                return Bounds {
                    lower: None,
                    upper: None,
                };
            }
        }
        Bounds { lower, upper }
    }

    pub fn add(&self, other: &Self) -> Self {
        fn add_lower(a: f32, b: f32) -> f32 {
            if a < 0.0 && b < 0.0 { a + b } else { a.max(b) }
        }
        fn add_upper(a: f32, b: f32) -> f32 {
            if a > 0.0 && b > 0.0 { a + b } else { a.min(b) }
        }
        let lower = Self::any_or(add_lower, self.lower, other.lower);
        let upper = Self::any_or(add_upper, self.upper, other.upper);
        Bounds { lower, upper }
    }

    pub fn contains(&self, value: f32) -> bool {
        if let Some(lower) = self.lower {
            if value < lower {
                return false;
            }
        }
        if let Some(upper) = self.upper {
            if value > upper {
                return false;
            }
        }
        true
    }
}

impl std::cmp::PartialEq<f64> for Bounds {
    fn eq(&self, other: &f64) -> bool {
        self.contains(*other as f32)
    }
}

impl std::cmp::PartialOrd<f64> for Bounds {
    fn partial_cmp(&self, other: &f64) -> Option<std::cmp::Ordering> {
        if self.contains(*other as f32) {
            Some(std::cmp::Ordering::Equal)
        } else if let Some(lower) = self.lower {
            if lower < (*other as f32) {
                Some(std::cmp::Ordering::Less)
            } else {
                Some(std::cmp::Ordering::Greater)
            }
        } else if let Some(upper) = self.upper {
            if upper > *other as f32 {
                Some(std::cmp::Ordering::Greater)
            } else {
                Some(std::cmp::Ordering::Less)
            }
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

impl std::cmp::PartialEq<Bounds> for f64 {
    fn eq(&self, other: &Bounds) -> bool {
        other.contains(*self as f32)
    }
}

impl std::cmp::PartialOrd<Bounds> for f64 {
    fn partial_cmp(&self, other: &Bounds) -> Option<std::cmp::Ordering> {
        other.partial_cmp(self).map(|o| o.reverse())
    }
}
