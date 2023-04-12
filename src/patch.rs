//! Patch

use std::fmt;

use crate::Diff;

/// Struct representing one patch operation.
#[derive(Debug, PartialEq, Default)]
pub struct Patch {
    pub diffs: Vec<Diff>,
    pub start1: usize,
    pub start2: usize,
    pub length1: usize,
    pub length2: usize,
}

impl fmt::Display for Patch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let coords1 = if self.length1 == 0 {
            format!("{},0", self.start1)
        } else if self.length1 == 1 {
            format!("{}", self.start1 + 1)
        } else {
            format!("{},{}", self.start1 + 1, self.length1)
        };

        let coords2 = if self.length2 == 0 {
            format!("{},0", self.start2)
        } else if self.length2 == 1 {
            format!("{}", self.start2 + 1)
        } else {
            format!("{},{}", self.start2 + 1, self.length2)
        };

        writeln!(f, "@@ -{coords1} +{coords2} @@")?;
        // TODO: Escape the body of the patch with %xx notation.
        for d in &self.diffs {
            let text = d.text().to_safe_encode();
            match d {
                Diff::Insert(_) => writeln!(f, "+{text}")?,
                Diff::Delete(_) => writeln!(f, "-{text}")?,
                Diff::Equal(_) => writeln!(f, " {text}")?,
            }
        }
        Ok(())
    }
}
