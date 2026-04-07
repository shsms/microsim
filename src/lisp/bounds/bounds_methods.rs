use crate::proto::common::metrics::Bounds;

impl Bounds {
    pub fn new(lower: Option<f32>, upper: Option<f32>) -> Self {
        Bounds { lower, upper }
    }

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
            if a <= 0.0 && b <= 0.0 {
                a + b
            } else {
                a.max(b)
            }
        }
        fn add_upper(a: f32, b: f32) -> f32 {
            if a >= 0.0 && b >= 0.0 {
                a + b
            } else {
                a.min(b)
            }
        }
        let lower = self
            .lower
            .and_then(|a| other.lower.map(|b| add_lower(a, b)));
        let upper = self
            .upper
            .and_then(|a| other.upper.map(|b| add_upper(a, b)));
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

#[cfg(test)]
mod tests {
    #[test]
    fn test_bounds_intersection() {
        let b1 = super::Bounds {
            lower: Some(0.0),
            upper: Some(10.0),
        };
        let b2 = super::Bounds {
            lower: Some(5.0),
            upper: Some(15.0),
        };
        let intersection = b1.intersect(&b2);
        assert_eq!(intersection.lower, Some(5.0));
        assert_eq!(intersection.upper, Some(10.0));

        let b3 = super::Bounds {
            lower: Some(11.0),
            upper: Some(20.0),
        };
        let intersection2 = b1.intersect(&b3);
        assert_eq!(intersection2.lower, None);
        assert_eq!(intersection2.upper, None);

        let b4 = super::Bounds {
            lower: None,
            upper: Some(8.0),
        };
        let intersection3 = b1.intersect(&b4);
        assert_eq!(intersection3.lower, Some(0.0));
        assert_eq!(intersection3.upper, Some(8.0));

        let b5 = super::Bounds {
            lower: Some(2.0),
            upper: None,
        };
        let intersection4 = b1.intersect(&b5);
        assert_eq!(intersection4.lower, Some(2.0));
        assert_eq!(intersection4.upper, Some(10.0));

        let b6 = super::Bounds {
            lower: None,
            upper: None,
        };
        let intersection5 = b1.intersect(&b6);
        assert_eq!(intersection5.lower, Some(0.0));
        assert_eq!(intersection5.upper, Some(10.0));
    }

    #[test]
    fn test_bounds_addition() {
        let b1 = super::Bounds {
            lower: Some(-5.0),
            upper: Some(5.0),
        };
        let b2 = super::Bounds {
            lower: Some(-3.0),
            upper: Some(3.0),
        };
        let addition = b1.add(&b2);
        assert_eq!(addition.lower, Some(-8.0));
        assert_eq!(addition.upper, Some(8.0));

        let b3 = super::Bounds {
            lower: Some(0.0),
            upper: Some(10.0),
        };
        let addition2 = b1.add(&b3);
        assert_eq!(addition2.lower, Some(-5.0));
        assert_eq!(addition2.upper, Some(15.0));

        let b4 = super::Bounds {
            lower: None,
            upper: Some(10.0),
        };
        let addition3 = b1.add(&b4);
        assert_eq!(addition3.lower, None);
        assert_eq!(addition3.upper, Some(15.0));

        let b5 = super::Bounds {
            lower: Some(-5.0),
            upper: None,
        };
        let addition4 = b1.add(&b5);
        assert_eq!(addition4.lower, Some(-10.0));
        assert_eq!(addition4.upper, None);

        let b6 = super::Bounds {
            lower: None,
            upper: None,
        };
        let addition5 = b1.add(&b6);
        assert_eq!(addition5.lower, None);
        assert_eq!(addition5.upper, None);
    }

    #[test]
    fn test_bounds_contains() {
        let b1 = super::Bounds {
            lower: Some(0.0),
            upper: Some(10.0),
        };
        assert!(b1.contains(5.0));
        assert!(b1.contains(0.0));
        assert!(b1.contains(10.0));
        assert!(!b1.contains(-1.0));
        assert!(!b1.contains(11.0));

        let b2 = super::Bounds {
            lower: None,
            upper: Some(10.0),
        };
        assert!(b2.contains(-100.0));
        assert!(b2.contains(0.0));
        assert!(b2.contains(10.0));
        assert!(!b2.contains(11.0));

        let b3 = super::Bounds {
            lower: Some(0.0),
            upper: None,
        };
        assert!(!b3.contains(-1.0));
        assert!(b3.contains(0.0));
        assert!(b3.contains(100.0));

        let b4 = super::Bounds {
            lower: None,
            upper: None,
        };
        assert!(b4.contains(-100.0));
        assert!(b4.contains(0.0));
        assert!(b4.contains(100.0));
    }

    #[test]
    fn test_bounds_partial_eq() {
        let b1 = super::Bounds {
            lower: Some(0.0),
            upper: Some(10.0),
        };
        assert_eq!(b1, 5.0);
        assert_eq!(b1, 0.0);
        assert_eq!(b1, 10.0);
        assert_ne!(b1, -1.0);
        assert_ne!(b1, 11.0);

        let b2 = super::Bounds {
            lower: None,
            upper: Some(10.0),
        };
        assert_eq!(b2, -100.0);
        assert_eq!(b2, 0.0);
        assert_eq!(b2, 10.0);
        assert_ne!(b2, 11.0);

        let b3 = super::Bounds {
            lower: Some(0.0),
            upper: None,
        };
        assert_ne!(b3, -1.0);
        assert_eq!(b3, 0.0);
        assert_eq!(b3, 100.0);

        let b4 = super::Bounds {
            lower: None,
            upper: None,
        };
        assert_eq!(b4, -100.0);
        assert_eq!(b4, 0.0);
        assert_eq!(b4, 100.0);
    }

    #[test]
    fn test_bounds_partial_ord() {
        let b1 = super::Bounds {
            lower: Some(0.0),
            upper: Some(10.0),
        };
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

        let b2 = super::Bounds {
            lower: None,
            upper: Some(10.0),
        };
        assert!(b2 == -100.0);
        assert!(b2 <= 10.0);
        assert!(b2 < 11.0);
        assert!(!(b2 > 10.0));
        assert!(!(b2 < -100.0));
        assert!(!(b2 >= 11.0));

        let b3 = super::Bounds {
            lower: Some(0.0),
            upper: None,
        };
        assert!(b3 > -10.0);
        assert!(b3 >= 100.0);
        assert!(b3 == 100.0);
        assert!(b3 <= 1000.0);
        assert!(!(b3 < 0.0));
        assert!(!(b3 > 100.0));
        assert!(!(b3 <= -10.0));

        let b4 = super::Bounds {
            lower: None,
            upper: None,
        };
        assert!(b4 >= -100.0);
        assert!(b4 <= 100.0);
        assert!(b4 == 0.0);
    }
}
