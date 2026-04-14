use crate::proto::common::metrics::Bounds;

impl Bounds {
    pub fn new(lower: Option<f32>, upper: Option<f32>) -> Self {
        Bounds { lower, upper }
    }

    fn map_or_any(f: impl FnOnce(f32, f32) -> f32, a: Option<f32>, b: Option<f32>) -> Option<f32> {
        match (a, b) {
            (Some(a), Some(b)) => Some(f(a, b)),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        }
    }

    pub fn intersect(&self, other: &Self) -> Self {
        let lower = Self::map_or_any(f32::max, self.lower, other.lower);
        let upper = Self::map_or_any(f32::min, self.upper, other.upper);
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

    pub fn add(&self, other: &Self) -> Vec<Self> {
        let intersection = self.intersect(other);
        if intersection.lower.is_none() && intersection.upper.is_none() {
            return vec![self.clone(), other.clone()];
        }
        let lower = self.lower.and_then(|a| {
            other.lower.map(|b| {
                if a <= 0.0 && b <= 0.0 {
                    a + b
                } else {
                    a.min(b)
                }
            })
        });
        let upper = self.upper.and_then(|a| {
            other.upper.map(|b| {
                if a >= 0.0 && b >= 0.0 {
                    a + b
                } else {
                    a.max(b)
                }
            })
        });
        vec![Bounds { lower, upper }]
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

    pub fn merge_if_overlapping(&self, other: &Self) -> Option<Self> {
        let intersection = self.intersect(other);

        if intersection.lower.is_some() || intersection.upper.is_some() {
            Some(Bounds {
                lower: self.lower.and_then(|a| other.lower.map(|b| a.min(b))),
                upper: self.upper.and_then(|a| other.upper.map(|b| a.max(b))),
            })
        } else {
            None
        }
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

#[cfg(test)]
mod tests {
    use super::Bounds;

    #[test]
    fn test_bounds_intersection() {
        let b1 = Bounds::new(Some(0.0), Some(10.0));
        let b2 = Bounds::new(Some(5.0), Some(15.0));
        assert_eq!(b1.intersect(&b2), Bounds::new(Some(5.0), Some(10.0)));

        let b2 = Bounds::new(Some(11.0), Some(20.0));
        assert_eq!(b1.intersect(&b2), Bounds::new(None, None));

        let b2 = Bounds::new(None, Some(8.0));
        assert_eq!(b1.intersect(&b2), Bounds::new(Some(0.0), Some(8.0)));

        let b2 = Bounds::new(Some(2.0), None);
        assert_eq!(b1.intersect(&b2), Bounds::new(Some(2.0), Some(10.0)));

        let b2 = Bounds::new(None, None);
        assert_eq!(b1.intersect(&b2), Bounds::new(Some(0.0), Some(10.0)));
    }

    #[test]
    fn test_bounds_addition() {
        let b1 = Bounds::new(Some(-5.0), Some(5.0));
        let b2 = Bounds::new(Some(-3.0), Some(3.0));
        assert_eq!(b1.add(&b2), vec![Bounds::new(Some(-8.0), Some(8.0))]);

        let b1 = Bounds::new(Some(-15.0), Some(-5.0));
        let b2 = Bounds::new(Some(-10.0), Some(-2.0));
        assert_eq!(b1.add(&b2), vec![Bounds::new(Some(-25.0), Some(-2.0))]);

        let b1 = Bounds::new(Some(5.0), Some(15.0));
        let b2 = Bounds::new(Some(2.0), Some(10.0));
        assert_eq!(b1.add(&b2), vec![Bounds::new(Some(2.0), Some(25.0))]);

        let b1 = Bounds::new(Some(5.0), Some(15.0));
        let b2 = Bounds::new(None, Some(10.0));
        assert_eq!(b1.add(&b2), vec![Bounds::new(None, Some(25.0))]);

        let b1 = Bounds::new(Some(5.0), Some(15.0));
        let b2 = Bounds::new(Some(-5.0), None);
        assert_eq!(b1.add(&b2), vec![Bounds::new(Some(-5.0), None)]);

        let b1 = Bounds::new(Some(5.0), Some(15.0));
        let b2 = Bounds::new(None, None);
        assert_eq!(b1.add(&b2), vec![Bounds::new(None, None)]);

        let b1 = Bounds::new(Some(-10.0), Some(-5.0));
        let b2 = Bounds::new(Some(5.0), Some(15.0));
        assert_eq!(b1.add(&b2), vec![b1, b2]);
    }

    #[test]
    fn test_bounds_contains() {
        let b1 = Bounds::new(Some(0.0), Some(10.0));
        assert!(b1.contains(5.0));
        assert!(b1.contains(0.0));
        assert!(b1.contains(10.0));
        assert!(!b1.contains(-1.0));
        assert!(!b1.contains(11.0));

        let b2 = Bounds::new(None, Some(10.0));
        assert!(b2.contains(-100.0));
        assert!(b2.contains(0.0));
        assert!(b2.contains(10.0));
        assert!(!b2.contains(11.0));

        let b3 = Bounds::new(Some(0.0), None);
        assert!(!b3.contains(-1.0));
        assert!(b3.contains(0.0));
        assert!(b3.contains(100.0));

        let b4 = Bounds::new(None, None);
        assert!(b4.contains(-100.0));
        assert!(b4.contains(0.0));
        assert!(b4.contains(100.0));
    }

    #[test]
    fn test_bounds_partial_eq() {
        let b1 = Bounds::new(Some(0.0), Some(10.0));
        assert_eq!(b1, 5.0);
        assert_eq!(b1, 0.0);
        assert_eq!(b1, 10.0);
        assert_ne!(b1, -1.0);
        assert_ne!(b1, 11.0);

        let b2 = Bounds::new(None, Some(10.0));
        assert_eq!(b2, -100.0);
        assert_eq!(b2, 0.0);
        assert_eq!(b2, 10.0);
        assert_ne!(b2, 11.0);

        let b3 = Bounds::new(Some(0.0), None);
        assert_ne!(b3, -1.0);
        assert_eq!(b3, 0.0);
        assert_eq!(b3, 100.0);

        let b4 = Bounds::new(None, None);
        assert_eq!(b4, -100.0);
        assert_eq!(b4, 0.0);
        assert_eq!(b4, 100.0);
    }

    #[test]
    fn test_bounds_partial_ord() {
        let b1 = Bounds::new(Some(0.0), Some(10.0));
        assert!(b1 > -1.0);
        assert!(b1 >= 0.0);
        assert!(b1 <= 10.0);
        assert!(b1 < 11.0);
        assert!(!(b1 <= -1.0));
        assert!(!(b1 < 0.0));
        assert!(!(b1 > 10.0));
        assert!(!(b1 >= 11.0));

        assert!(-1.0 < b1);
        assert!(0.0 <= b1);
        assert!(10.0 >= b1);
        assert!(11.0 > b1);
        assert!(!(-1.0 > b1));
        assert!(!(0.0 < b1));
        assert!(!(10.0 < b1));
        assert!(!(11.0 <= b1));

        let b2 = Bounds::new(None, Some(10.0));
        assert!(b2 == -100.0);
        assert!(b2 <= 10.0);
        assert!(b2 < 11.0);
        assert!(!(b2 > 10.0));
        assert!(!(b2 < -100.0));
        assert!(!(b2 >= 11.0));

        let b3 = Bounds::new(Some(0.0), None);
        assert!(b3 > -10.0);
        assert!(b3 >= 100.0);
        assert!(b3 == 100.0);
        assert!(b3 <= 1000.0);
        assert!(!(b3 < 0.0));
        assert!(!(b3 > 100.0));
        assert!(!(b3 <= -10.0));

        let b4 = Bounds::new(None, None);
        assert!(b4 >= -100.0);
        assert!(b4 <= 100.0);
        assert!(b4 == 0.0);
    }

    #[test]
    fn test_bounds_merge() {
        let b1 = Bounds::new(Some(0.0), Some(10.0));
        let b2 = Bounds::new(Some(5.0), Some(15.0));

        let merged = b1.merge_if_overlapping(&b2).unwrap();
        assert_eq!(merged, Bounds::new(Some(0.0), Some(15.0)));

        let b3 = Bounds::new(Some(10.0), Some(20.0));
        let merged2 = b1.merge_if_overlapping(&b3).unwrap();
        assert_eq!(merged2, Bounds::new(Some(0.0), Some(20.0)));

        let b4 = Bounds::new(Some(11.0), Some(20.0));
        assert!(b1.merge_if_overlapping(&b4).is_none());

        let b5 = Bounds::new(None, Some(10.0));
        let merged3 = b1.merge_if_overlapping(&b5).unwrap();
        assert_eq!(merged3, Bounds::new(None, Some(10.0)));

        let b6 = Bounds::new(Some(0.0), None);
        let merged4 = b1.merge_if_overlapping(&b6).unwrap();
        assert_eq!(merged4, Bounds::new(Some(0.0), None));

        let b7 = Bounds::new(None, None);
        let merged5 = b1.merge_if_overlapping(&b7).unwrap();
        assert_eq!(merged5, Bounds::new(None, None));
    }
}
