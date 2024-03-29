use percent_encoding::{percent_decode, utf8_percent_encode, AsciiSet, NON_ALPHANUMERIC};
use std::{
    borrow::Borrow,
    fmt, mem,
    ops::{self, Deref, DerefMut},
};

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

/// A String with char as underlying type.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct Chars(Vec<char>);

impl Chars {
    pub const fn new() -> Self {
        Chars(Vec::new())
    }

    pub fn push(&mut self, c: char) {
        self.0.push(c)
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn take(&mut self) -> Self {
        Chars(mem::take(&mut self.0))
    }

    pub fn to_safe_decode(&self) -> Result<Self, ()> {
        let tmp: &[u8] = &self[..].iter().map(|c| *c as u8).collect::<Vec<_>>();
        match percent_decode(tmp).decode_utf8() {
            Err(_) => Err(()),
            Ok(s) => Ok(Chars::from(s.to_string())),
        }
    }

    pub fn to_safe_encode(&self) -> String {
        utf8_percent_encode(&Into::<String>::into(self), &PERCENT_ENCONDING_SET).to_string()
    }

    /// Slice with negative index support
    pub(crate) fn slice_to(&self, i: isize) -> &[char] {
        if i < 0 {
            &self[..self.len() - i.unsigned_abs()]
        } else {
            &self[..i as usize]
        }
    }

    pub(crate) fn slice_from(&self, i: isize) -> &[char] {
        if i < 0 {
            &self[self.len() - i.unsigned_abs()..]
        } else {
            &self[i as usize..]
        }
    }

    /// Translate char map
    pub(crate) fn translate<T: Clone>(&self, item_array: &[T]) -> Vec<T> {
        self.iter()
            .map(|c| {
                let mut idx = *c as usize;
                if idx > 55296 {
                    // in hex: 0xD800
                    idx -= 2048; // in hex: 0x800
                }
                item_array[idx].clone()
            })
            .collect()
    }
}

impl PartialEq<[char]> for Chars {
    fn eq(&self, other: &[char]) -> bool {
        self.0 == other
    }
}

impl PartialEq<[char]> for &Chars {
    fn eq(&self, other: &[char]) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<Vec<char>> for Chars {
    fn eq(&self, other: &Vec<char>) -> bool {
        self.0 == *other
    }
}

impl PartialEq<Vec<char>> for &Chars {
    fn eq(&self, other: &Vec<char>) -> bool {
        &self.0 == other
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

impl From<Vec<char>> for Chars {
    fn from(s: Vec<char>) -> Self {
        Chars(s)
    }
}

impl From<&Chars> for Chars {
    fn from(s: &Chars) -> Self {
        s.clone()
    }
}

impl From<Chars> for String {
    fn from(s: Chars) -> Self {
        s.0.iter().collect()
    }
}

impl From<&Chars> for String {
    fn from(s: &Chars) -> Self {
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
        self
    }
}

impl AsRef<[char]> for Chars {
    fn as_ref(&self) -> &[char] {
        self
    }
}

impl ops::AddAssign<&[char]> for Chars {
    fn add_assign(&mut self, other: &[char]) {
        self.0.extend_from_slice(other)
    }
}

impl ops::AddAssign<&[char]> for &mut Chars {
    fn add_assign(&mut self, other: &[char]) {
        self.0.extend_from_slice(other)
    }
}

impl ops::Add<&[char]> for Chars {
    type Output = Self;

    fn add(mut self, other: &[char]) -> Self {
        self += other;
        self
    }
}

/*
impl ops::AddAssign for Chars {
    fn add_assign(&mut self, other: Chars) {
        self.0.extend(other.0)
    }
}
*/

/*
impl std::slice::Concat<&Chars> for Chars {
    type Output = Chars;

    fn concat(&self) -> Self::Output {
        let mut result = Chars::new();
        for s in self {
            result += s;
        }
        result
    }
}
*/
