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
        Self::squash(result)
    }

    pub fn add(&self, other: &Self) -> Self {
        match (self.0.as_slice(), &other.0.as_slice()) {
            ([a], []) | ([], [a]) => Self(vec![a.clone()]),
            ([a], [b]) => Self(a.add(b)),
            ([a_first, .., a_last], [b]) | ([b], [a_first, .., a_last]) => {
                let mut result = a_first.add(b);
                result.extend(a_last.add(b).into_iter());
                Self::squash(result)
            }
            ([a_first, .., a_last], [b_first, .., b_last]) => {
                let mut result = a_first.add(b_first);
                result.extend(a_last.add(b_last).into_iter());
                Self::squash(result)
            }
            _ => Self(vec![]), // TODO: Handle more complex cases if needed
        }
    }

    pub fn squash(mut input: Vec<Bounds>) -> Self {
        input.sort_by(|a, b| {
            a.lower
                .unwrap_or(f32::MIN)
                .partial_cmp(&b.lower.unwrap_or(f32::MIN))
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        if input.is_empty() {
            return Self(input);
        }

        let mut squashed = Vec::new();
        let mut current = input[0].clone();

        for next in &input[1..] {
            if let Some(merged_bounds) = current.merge_if_overlapping(next) {
                current = merged_bounds;
            } else {
                squashed.push(current);
                current = next.clone();
            }
        }
        squashed.push(current);

        Self(squashed)
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

        let vb2 = VecBounds::new(vec![
            Bounds::new(Some(-20.0), None),
            Bounds::new(None, Some(40.0)),
        ]);
        let intersection = vb1.intersect(&vb2);
        assert_eq!(
            intersection.0,
            vec![
                Bounds::new(Some(-30.0), Some(-10.0)),
                Bounds::new(Some(10.0), Some(30.0)),
            ]
        );

        let vb2 = VecBounds::new(vec![
            Bounds::new(None, Some(-20.0)),
            Bounds::new(Some(20.0), None),
        ]);
        let intersection = vb1.intersect(&vb2);
        assert_eq!(
            intersection.0,
            vec![
                Bounds::new(Some(-30.0), Some(-20.0)),
                Bounds::new(Some(20.0), Some(30.0)),
            ]
        );

        let vb2 = VecBounds::new(vec![Bounds::new(Some(-25.0), Some(25.0))]);
        let intersection = vb1.intersect(&vb2);
        assert_eq!(
            intersection.0,
            vec![
                Bounds::new(Some(-25.0), Some(-10.0)),
                Bounds::new(Some(10.0), Some(25.0)),
            ]
        );

        let vb2 = VecBounds::new(vec![Bounds::new(Some(-5.0), Some(5.0))]);
        let intersection = vb1.intersect(&vb2);
        assert_eq!(intersection.0, Vec::<Bounds>::new());
    }

    #[test]
    fn test_vec_bounds_add() {
        let b1 = VecBounds::new(vec![Bounds::new(Some(-5.0), Some(5.0))]);
        let b2 = VecBounds::new(vec![
            Bounds::new(Some(-5.0), Some(-2.0)),
            Bounds::new(Some(2.0), Some(5.0)),
        ]);
        let result = b1.add(&b2);
        assert_eq!(result.0, vec![Bounds::new(Some(-10.0), Some(10.0))]);

        let b1 = VecBounds::new(vec![Bounds::new(Some(-5.0), Some(-1.0))]);
        let b2 = VecBounds::new(vec![
            Bounds::new(Some(-5.0), Some(-2.0)),
            Bounds::new(Some(2.0), Some(5.0)),
        ]);
        let result = b1.add(&b2);
        assert_eq!(
            result.0,
            vec![
                Bounds::new(Some(-10.0), Some(-1.0)),
                Bounds::new(Some(2.0), Some(5.0))
            ]
        );
    }
}
