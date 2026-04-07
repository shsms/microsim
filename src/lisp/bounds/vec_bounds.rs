use tulisp::{Error, Shared, TulispConvertible, TulispObject};

use crate::proto::common::metrics::Bounds;

#[derive(Debug, Clone)]
pub(crate) struct VecBounds(pub(crate) Vec<Bounds>);

impl std::fmt::Display for VecBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<bounds: {:?}>", self.0)
    }
}

impl TulispConvertible for VecBounds {
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
        match value.as_any() {
            Ok(value) => match value.downcast_ref::<VecBounds>() {
                Some(v) => Ok(v.clone()),
                None => Err(Error::type_mismatch(format!(
                    "Expected VecBounds, got {value}"
                ))),
            },
            Err(_) => Err(Error::type_mismatch(format!(
                "Expected VecBounds, got {value}"
            ))),
        }
    }

    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_bounds_contains() {
        let vb = VecBounds::new(vec![
            Bounds::new(Some(-30.0), Some(-10.0)),
            Bounds::new(Some(10.0), Some(30.0)),
        ]);

        assert!(vb.contains(-20.0));
        assert!(vb.contains(20.0));
        assert!(!vb.contains(-5.0));
        assert!(!vb.contains(5.0));
        assert!(!vb.contains(-50.0));
        assert!(!vb.contains(50.0));

        let vb2 = VecBounds::new(vec![
            Bounds::new(Some(-20.0), None),
            Bounds::new(None, Some(40.0)),
        ]);

        assert!(vb2.contains(-50.0));
        assert!(vb2.contains(-20.0));
        assert!(vb2.contains(30.0));
        assert!(vb2.contains(10.0));
        assert!(vb2.contains(50.0));

        let vb3 = VecBounds::new(vec![
            Bounds::new(None, Some(-10.0)),
            Bounds::new(Some(10.0), None),
        ]);

        assert!(vb3.contains(-20.0));
        assert!(vb3.contains(20.0));
        assert!(!vb3.contains(-5.0));
        assert!(!vb3.contains(5.0));
        assert!(vb3.contains(-50.0));
        assert!(vb3.contains(50.0));
    }

    #[test]
    fn test_vec_bounds_intersect() {
        let vb1 = VecBounds::new(vec![
            Bounds::new(Some(-30.0), Some(-10.0)),
            Bounds::new(Some(10.0), Some(30.0)),
        ]);

        let vb2 = VecBounds::new(vec![
            Bounds::new(Some(-20.0), Some(0.0)),
            Bounds::new(Some(20.0), Some(40.0)),
        ]);

        let intersection = vb1.intersect(&vb2);
        assert_eq!(
            intersection.0,
            vec![
                Bounds::new(Some(-20.0), Some(-10.0)),
                Bounds::new(Some(20.0), Some(30.0)),
            ]
        );

        let vb3 = VecBounds::new(vec![
            Bounds::new(Some(-20.0), None),
            Bounds::new(None, Some(40.0)),
        ]);

        let intersection2 = vb1.intersect(&vb3);
        assert_eq!(
            intersection2.0,
            vec![
                Bounds::new(Some(-30.0), Some(-10.0)),
                Bounds::new(Some(-20.0), Some(-10.0)),
                Bounds::new(Some(10.0), Some(30.0)),
                Bounds::new(Some(10.0), Some(30.0)),
            ]
        );

        let vb4 = VecBounds::new(vec![
            Bounds::new(None, Some(-20.0)),
            Bounds::new(Some(20.0), None),
        ]);

        let intersection3 = vb1.intersect(&vb4);
        assert_eq!(
            intersection3.0,
            vec![
                Bounds::new(Some(-30.0), Some(-20.0)),
                Bounds::new(Some(20.0), Some(30.0)),
            ]
        );
    }
}
