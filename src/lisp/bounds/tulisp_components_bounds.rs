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
        let squashed = self.squash();
        if self.augmented.is_empty() {
            write!(f, "{squashed}")
        } else {
            write!(
                f,
                "{squashed} (rated {}, +{} augmented)",
                self.rated_bounds,
                self.augmented.len()
            )
        }
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
        if self.augmented.is_empty() {
            return self.rated_bounds.clone();
        }
        // Per microgrid.proto AugmentElectricalComponentBounds semantics,
        // multiple augmented bound ranges are merged (union), not intersected.
        // The rated bounds then act as a hard physical ceiling.
        let mut all_aug: Vec<crate::proto::common::metrics::Bounds> = Vec::new();
        for (_, b, _) in self.augmented.iter() {
            all_aug.extend(b.0.iter().cloned());
        }
        let unioned = VecBounds::squash(all_aug);
        self.rated_bounds.intersect(&unioned)
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
