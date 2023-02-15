//! Patch

use std::fmt;

use crate::Diff;
use percent_encoding::{utf8_percent_encode, AsciiSet, NON_ALPHANUMERIC};

// "!~*'();/?:@&=+$,# "
const PERCENT_ENCONDING_SET: AsciiSet = NON_ALPHANUMERIC
    .remove(b'!')
    .remove(b'~')
    .remove(b'*')
    .remove(b'\'')
    .remove(b'(')
    .remove(b')')
    .remove(b';')
    .remove(b'/')
    .remove(b'?')
    .remove(b':')
    .remove(b'@')
    .remove(b'&')
    .remove(b'=')
    .remove(b'+')
    .remove(b'$')
    .remove(b',')
    .remove(b'#')
    .remove(b' ');

//const PERCENT_ENCONDING_SET: AsciiSet = CONTROLS.remove(b'!')
//  .remove(b'~')
//.remove(b'*')

/// Struct representing one patch operation.
#[derive(Debug, PartialEq)]
pub struct PatchObj {
    pub diffs: Vec<Diff>,
    pub start1: Option<usize>,
    pub start2: Option<usize>,
    pub length1: usize,
    pub length2: usize,
}

impl fmt::Display for PatchObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let coords1 = if self.length1 == 0 {
            format!("{},0", self.start1.unwrap())
        } else if self.length1 == 1 {
            format!("{}", self.start1.unwrap() + 1)
        } else {
            format!("{},{}", self.start1.unwrap() + 1, self.length1)
        };

        let coords2 = if self.length2 == 0 {
            format!("{},0", self.start2.unwrap())
        } else if self.length2 == 1 {
            format!("{}", self.start2.unwrap() + 1)
        } else {
            format!("{},{}", self.start2.unwrap() + 1, self.length2)
        };

        writeln!(f, "@@ -{coords1} +{coords2} @@")?;
        // TODO: Escape the body of the patch with %xx notation.
        for d in &self.diffs {
            let text = d.text().to_string();
            let encoded = utf8_percent_encode(&text, &PERCENT_ENCONDING_SET).to_string();
            match d {
                Diff::Insert(_) => writeln!(f, "+{encoded}")?,
                Diff::Delete(_) => writeln!(f, "-{encoded}")?,
                Diff::Equal(_) => writeln!(f, " {encoded}")?,
            }
        }
        Ok(())
    }
}
