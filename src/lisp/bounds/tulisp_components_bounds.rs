use std::collections::VecDeque;

use tulisp::{Error, Shared, TulispConvertible, TulispObject};

use crate::lisp::{bounds::VecBounds, time::TulispDateTime};

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

#[cfg(test)]
mod tests {
    use crate::lisp::bounds::{TulispComponentBounds, VecBounds};
    use crate::lisp::time::TulispDateTime;
    use crate::proto::common::metrics::Bounds;

    #[test]
    fn test_squash() {
        let mut component_bounds =
            TulispComponentBounds::new(VecBounds(vec![Bounds::new(Some(-200.0), Some(200.0))]));

        component_bounds.augmented.push_back((
            TulispDateTime::now(),
            VecBounds(vec![
                Bounds::new(Some(-250.0), Some(-50.0)),
                Bounds::new(Some(50.0), Some(150.0)),
            ]),
            3600,
        ));

        let squashed = component_bounds.squash();
        assert_eq!(
            squashed.0,
            vec![
                Bounds::new(Some(-200.0), Some(-50.0)),
                Bounds::new(Some(50.0), Some(150.0))
            ]
        );
    }
}
