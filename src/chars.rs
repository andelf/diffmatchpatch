use std::{
    borrow::Borrow,
    fmt,
    ops::{Deref, DerefMut},
};

/// A String with char
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Chars(Vec<char>);

impl Chars {
    pub fn new() -> Self {
        Chars(Vec::new())
    }
}

impl PartialEq<[char]> for Chars {
    fn eq(&self, other: &[char]) -> bool {
        self.0 == other
    }
}

impl From<String> for Chars {
    fn from(s: String) -> Self {
        From::from(&*s)
    }
}

impl From<&str> for Chars {
    fn from(s: &str) -> Self {
        Chars(s.chars().collect())
    }
}

impl From<&[char]> for Chars {
    fn from(s: &[char]) -> Self {
        Chars(s.to_vec())
    }
}

impl From<Chars> for String {
    fn from(s: Chars) -> Self {
        s.0.iter().collect()
    }
}

impl From<Chars> for Vec<char> {
    fn from(s: Chars) -> Self {
        s.0
    }
}

impl fmt::Debug for Chars {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.iter().collect::<String>())
    }
}

impl fmt::Display for Chars {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter().collect::<String>())
    }
}

impl Deref for Chars {
    type Target = Vec<char>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Chars {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Borrow<[char]> for Chars {
    fn borrow(&self) -> &[char] {
        &*self
    }
}
