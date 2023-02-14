use core::{char, fmt};
use regex::{internal::Char, Regex};
use std::{
    collections::HashMap,
    iter::FromIterator,
    result::Result,
    time::{Duration, Instant},
};
use urlencoding::{decode, encode};

pub use chars::Chars;

pub mod chars;

pub trait ToChars {
    fn to_chars(&self) -> Vec<char>;
}

impl ToChars for String {
    fn to_chars(&self) -> Vec<char> {
        self.chars().collect()
    }
}

impl ToChars for &str {
    fn to_chars(&self) -> Vec<char> {
        self.chars().collect()
    }
}

pub enum LengthUnit {
    UnicodeScalar,
    UTF16,
}

/// Diff Match and Patch methods
pub struct DiffMatchPatch {
    /// Time duration to map a diff before giving up (None for infinity).
    pub diff_timeout: Option<Duration>,
    /// Cost of an empty edit operation in terms of edit characters.
    pub edit_cost: i32,
    /// How far to search for a match (0 = exact location, 1000+ = broad match).
    /// A match this many characters away from the expected location will add
    /// 1.0 to the score (0.0 is a perfect match).*/
    pub match_distance: i32,
    /// Chunk size for context length.
    pub patch_margin: i32,
    /// The number of bits in an int.
    pub match_maxbits: i32,
    /// At what point is no match declared (0.0 = perfection, 1.0 = very loose).
    pub match_threshold: f32,
    // When deleting a large block of text (over ~64 characters), how close do
    // the contents have to be to match the expected contents. (0.0 = perfection,
    // 1.0 = very loose).  Note that Match_Threshold controls how closely the
    // end points of a delete need to match.*/
    pub patch_delete_threshold: f32,
}

pub fn new() -> DiffMatchPatch {
    DiffMatchPatch {
        diff_timeout: None,
        patch_delete_threshold: 0.5,
        edit_cost: 0,
        match_distance: 1000,
        patch_margin: 4,
        match_maxbits: 32,
        match_threshold: 0.5,
    }
}

/// The data structure representing a diff
#[derive(Debug, PartialEq, Clone)]
pub enum Diff {
    Delete(Chars),
    Insert(Chars),
    Equal(Chars),
}

impl Diff {
    pub fn text(&self) -> &Chars {
        match self {
            Diff::Delete(text) => text,
            Diff::Insert(text) => text,
            Diff::Equal(text) => text,
        }
    }

    pub fn text_mut(&mut self) -> &mut Chars {
        match self {
            Diff::Delete(text) => text,
            Diff::Insert(text) => text,
            Diff::Equal(text) => text,
        }
    }
}

#[derive(Debug)]
pub struct Patch {
    pub diffs: Vec<Diff>,
    pub start1: i32,
    pub start2: i32,
    pub length1: i32,
    pub length2: i32,
}

impl Patch {
    // A new diff patch object created.
    pub fn new(diffs: Vec<Diff>, start1: i32, start2: i32, length1: i32, length2: i32) -> Patch {
        Patch {
            diffs,
            start1,
            start2,
            length1,
            length2,
        }
    }
}

impl PartialEq for Patch {
    // it will return if two patch objects are equal or not.
    fn eq(&self, other: &Self) -> bool {
        (self.diffs == other.diffs)
            & (self.start1 == other.start1)
            & (self.start2 == other.start2)
            & (self.length1 == other.length1)
            & (self.length2 == other.length2)
    }
}

fn min(x: i32, y: i32) -> i32 {
    // return minimum element.
    if x > y {
        return y;
    }
    x
}

fn min1(x: f32, y: f32) -> f32 {
    // return minimum element.
    if x > y {
        return y;
    }
    x
}

fn max(x: i32, y: i32) -> i32 {
    // return maximum element.
    if x > y {
        return x;
    }
    y
}

// it will return the first index of a character after a index or return -1 if
// not found.
fn find_char(cha: char, text: &[char], start: usize) -> isize {
    text[start..]
        .iter()
        .enumerate()
        .find(|(_i, &c)| c == cha)
        .map(|(i, _)| (i + start) as isize)
        .unwrap_or(-1)
}

trait StringView {
    fn len(&self) -> usize;
    fn slice(&self, range: std::ops::Range<usize>) -> Result<String, std::string::FromUtf16Error>;
}

impl DiffMatchPatch {
    pub fn new() -> Self {
        DiffMatchPatch {
            diff_timeout: None,
            patch_delete_threshold: 0.5,
            edit_cost: 0,
            match_distance: 1000,
            patch_margin: 4,
            match_maxbits: 32,
            match_threshold: 0.5,
        }
    }

    /**
    Find the first index after a specific index in text1 where patern is present.

    Args:
        text1: Parent chars.
        text2: Patern chars.
        ind: index after which we have to find the patern.

    Returns:
        the first index where patern is found or -1 if not found.
    */
    fn kmp(&self, text1: &[char], text2: &[char], ind: usize) -> Option<usize> {
        if text2.is_empty() {
            return Some(ind);
        }
        if text1.is_empty() {
            return None;
        }
        let len1 = text1.len();
        let len2 = text2.len();
        let mut patern: Vec<usize> = Vec::new();
        patern.push(0);
        let mut len = 0;
        let mut i = 1;

        // Preprocess the pattern
        while i < len2 {
            if text2[i] == text2[len] {
                len += 1;
                patern.push(len);
                i += 1;
            } else if len == 0 {
                patern.push(0);
                i += 1;
            } else {
                len = patern[len - 1];
            }
        }
        i = ind;
        len = 0;
        while i < len1 {
            if text1[i] == text2[len] {
                len += 1;
                i += 1;
                if len == len2 {
                    return Some(i - len);
                }
            } else if len == 0 {
                i += 1;
            } else {
                len = patern[len - 1];
            }
        }
        None
    }

    fn rkmp(&mut self, text1: &Vec<char>, text2: &Vec<char>, ind: usize) -> Option<usize> {
        /*
        Find the last index before a specific index in text1 where patern is present.

        Args:
            text1: Parent chars.
            text2: Patern chars.
            ind: index just before we have to find the patern.

        Returns:
            the last index where patern is found or -1 if not found.
        */
        if text2.is_empty() {
            return Some(ind);
        }
        if text1.is_empty() {
            return None;
        }
        let len2 = text2.len();
        let mut patern: Vec<usize> = Vec::new();
        patern.push(0);
        let mut len = 0;
        let mut i = 1;

        // Preprocess the pattern
        while i < len2 {
            if text2[i] == text2[len] {
                len += 1;
                patern.push(len);
                i += 1;
            } else if len == 0 {
                patern.push(0);
                i += 1;
            } else {
                len = patern[len - 1];
            }
        }
        i = 0;
        len = 0;
        let mut ans = usize::MAX;
        while i <= ind {
            if text1[i] == text2[len] {
                len += 1;
                i += 1;
                if len == len2 {
                    ans = i - len;
                    len = patern[len - 1];
                }
            } else if len == 0 {
                i += 1;
            } else {
                len = patern[len - 1];
            }
        }
        if ans == usize::MAX {
            None
        } else {
            Some(ans)
        }
    }

    /**
    Rehydrate the text in a diff from a string of line hashes to real lines
    of text.

    Args:
        diffs: Vector of diffs as changes.
        lineArray: Vector of unique strings.
    */
    pub fn diff_chars_to_lines(&self, diffs: &mut Vec<Diff>, line_array: &Vec<String>) {
        for diff in diffs {
            let mut text: String = "".to_string();
            let text1 = diff.text().clone();
            let chars: Vec<char> = text1.into();
            for j in 0..chars.len() {
                text += line_array[chars[j] as usize].as_str();
            }
            *diff.text_mut() = text.into();
        }
    }

    /**
      Determine the common prefix of two strings.

      Args:
          text1: First strings.
          text2: Second strings.

      Returns:
          The number of characters common to the start of each chars.
    */
    pub fn diff_common_prefix<T: AsRef<[char]>>(&self, text1: T, text2: T) -> usize {
        let text1 = text1.as_ref();
        let text2 = text2.as_ref();
        // Quick check for common null cases
        if text1.is_empty() || text2.is_empty() || text1[0] != text2[0] {
            return 0;
        }
        // Binary search.
        let pointermax = usize::min(text1.len(), text2.len());
        let mut pointerstart = 0;
        while pointerstart < pointermax {
            if text1[pointerstart as usize] == text2[pointerstart as usize] {
                pointerstart += 1;
            } else {
                return pointerstart;
            }
        }
        pointermax
    }

    /**
      Determine the common suffix of two strings.

      Args:
          text1: First chars.
          text2: Second chars.

      Returns:
          The number of characters common to the end of each chars.
    */
    pub fn diff_common_suffix<T: AsRef<[char]>>(&self, text1: T, text2: T) -> usize {
        let text1 = text1.as_ref();
        let text2 = text2.as_ref();

        if text1.is_empty() || text2.is_empty() {
            return 0;
        }
        let mut pointer_1 = (text1.len() - 1) as isize;
        let mut pointer_2 = (text2.len() - 1) as isize;
        let mut len = 0;
        while pointer_1 >= 0 && pointer_2 >= 0 {
            if text1[pointer_1 as usize] == text2[pointer_2 as usize] {
                len += 1;
            } else {
                break;
            }
            pointer_1 -= 1;
            pointer_2 -= 1;
        }
        len
    }

    /*
      Determine if the suffix of one chars is the prefix of another.

      Args:
          text1 First chars.
          text2 Second chars.

      Returns:
          The number of characters common to the end of the first
          chars and the start of the second chars.
    */
    pub fn diff_common_overlap<T: AsRef<[char]>>(&self, text1: T, text2: T) -> usize {
        let text1 = text1.as_ref();
        let text2 = text2.as_ref();

        // Eliminate the null case.
        if text1.is_empty() || text2.is_empty() {
            return 0;
        }

        let text1_trunc;
        let text2_trunc;
        let len = text1.len().min(text2.len());

        // Truncate the longer chars.
        if text1.len() > text2.len() {
            text1_trunc = &text1[(text1.len() - text2.len())..];
            text2_trunc = &text2[..];
        } else {
            text1_trunc = &text1[..];
            text2_trunc = &text2[0..text1.len()];
        }
        let mut best = 0;
        let mut length = 1;
        // Quick check for the worst case.
        if text1_trunc == text2_trunc {
            return len;
        }
        // Start by looking for a single character match
        // and increase length until no match is found.
        // Performance analysis: https://neil.fraser.name/news/2010/11/04/
        loop {
            let patern = &text1_trunc[(len as usize - length)..(len as usize)];
            if let Some(found) = self.kmp(text2_trunc, patern, 0) {
                length += found;
                if found == 0 {
                    best = length;
                    length += 1;
                }
            } else {
                return best;
            }
        }
    }

    /**
    Do the two texts share a substring which is at least half the length of
    the longer text?
    This speedup can produce non-minimal diffs.

    Args:
        text1: First chars.
        text2: Second chars.

    Returns:
        Five element Vector, containing the prefix of text1, the suffix of text1,
        the prefix of text2, the suffix of text2 and the common middle.  Or empty vector
        if there was no match.
    */
    pub fn diff_half_match<'a, 'b>(
        &'b self,
        text1: &'a [char],
        text2: &'a [char],
    ) -> Option<Vec<&'a [char]>> {
        if self.diff_timeout.is_none() {
            // Don't risk returning a non-optimal diff if we have unlimited time.
            return None;
        }

        let text1 = text1.as_ref();
        let text2 = text2.as_ref();

        let (long_text, short_text) = if text1.len() > text2.len() {
            (text1, text2)
        } else {
            (text2, text1)
        };
        if long_text.len() < 4 || short_text.len() * 2 < long_text.len() {
            return None;
        }

        let mut hm: Vec<&[char]>;
        // First check if the second quarter is the seed for a half-match.
        let hm1 = self.diff_half_matchi(long_text, short_text, (long_text.len() + 3) / 4);
        // Check again based on the third quarter.
        let hm2 = self.diff_half_matchi(long_text, short_text, (long_text.len() + 1) / 2);

        if hm1.is_empty() && hm2.is_empty() {
            return None;
        } else if hm1.is_empty() {
            hm = hm2;
        } else if hm2.is_empty() {
            hm = hm1;
        } else {
            // Both matched.  Select the longest.
            hm = if hm1[4].len() > hm2[4].len() {
                hm1
            } else {
                hm2
            };
        }
        if text1.len() > text2.len() {
            return Some(hm);
        }
        let mut temp2 = hm[0].clone();
        let mut temp3 = hm[2].clone();
        hm[0] = temp3;
        hm[2] = temp2;
        temp2 = hm[1].clone();
        temp3 = hm[3].clone();
        hm[1] = temp3;
        hm[3] = temp2;
        Some(hm)
    }

    /**
    Does a substring of shorttext exist within longtext such that the
    substring is at least half the length of longtext?
    Closure, but does not reference any external variables.

    Args:
        longtext: Longer chars.
        shorttext: Shorter chars.
        i: Start index of quarter length substring within longtext.

    Returns:
        Five element vector, containing the prefix of longtext, the suffix of
        longtext, the prefix of shorttext, the suffix of shorttext and the
        common middle.  Or empty vector if there was no match.
    */
    fn diff_half_matchi<'a, 'b>(
        &'b self,
        long_text: &'a [char],
        short_text: &'a [char],
        i: usize,
    ) -> Vec<&'a [char]> {
        let long_len = long_text.len();
        let seed =
            Vec::from_iter(long_text[(i as usize)..(i as usize + long_len / 4)].iter().cloned());
        let mut best_common: &[char] = &[];
        let mut best_longtext_a: &[char] = &[];
        let mut best_longtext_b: &[char] = &[];
        let mut best_shorttext_a: &[char] = &[];
        let mut best_shorttext_b: &[char] = &[];
        let mut j = self.kmp(short_text, &seed, 0);
        while j.is_some() {
            let prefix_length =
                self.diff_common_prefix(&long_text[(i as usize)..], &short_text[j.unwrap()..]);
            let suffix_length =
                self.diff_common_suffix(&long_text[..(i as usize)], &short_text[..j.unwrap()]);
            if best_common.len() < suffix_length as usize + prefix_length as usize {
                best_common = &short_text
                    [(j.unwrap() - suffix_length as usize)..(j.unwrap() + prefix_length as usize)];
                best_longtext_a = &long_text[..(i as usize - suffix_length)];
                best_longtext_b = &long_text[(i as usize + prefix_length)..];
                best_shorttext_a = &short_text[..(j.unwrap() - suffix_length)];
                best_shorttext_b = &short_text[(j.unwrap() + prefix_length)..];
            }
            j = self.kmp(short_text, &seed, j.unwrap() + 1);
        }
        if best_common.len() * 2 >= long_text.len() {
            return vec![
                best_longtext_a,
                best_longtext_b,
                best_shorttext_a,
                best_shorttext_b,
                best_common,
            ];
        }
        vec![]
    }

    /**
    Split two texts into an array of strings.  Reduce the texts to a string
    of hashes where each Unicode character represents one line.

    Args:
        text1: First chars.
        text2: Second chars.

    Returns:
        Three element tuple, containing the encoded text1, the encoded text2 and
        the array of unique strings.  The zeroth element of the array of unique
        strings is intentionally blank.
    */
    pub fn diff_lines_to_chars(&self, text1: &str, text2: &str) -> (Chars, Chars, Vec<String>) {
        let mut linearray: Vec<String> = vec!["".into()];
        let mut linehash: HashMap<String, u32> = HashMap::new();
        let chars1 = self.diff_lines_tochars_munge(text1, &mut linearray, &mut linehash);
        //       let mut dmp = DiffMatchPatch::new();
        let chars2 = self.diff_lines_tochars_munge(text2, &mut linearray, &mut linehash);
        (chars1, chars2, linearray)
    }

    /**
    Split a text into an array of strings.  Reduce the texts to a string
    of hashes where each Unicode character represents one line.
    Modifies linearray and linehash through being a closure.

    Args:
        text: chars to encode.

    Returns:
        Encoded string.
    */
    pub fn diff_lines_tochars_munge<'a>(
        &self,
        text: &str,
        linearray: &'a mut Vec<String>,
        linehash: &'a mut HashMap<String, u32>,
    ) -> Chars {
        let mut chars = Chars::new();
        // Walk the text, pulling out a substring for each line.
        // text.split('\n') would would temporarily double our memory footprint.
        // Modifying text would create many large strings to garbage collect.
        for line in text.split_inclusive('\n') {
            if linehash.contains_key(line) {
                if let Some(ch) = char::from_u32(linehash[line]) {
                    chars.push(ch);
                } else {
                    panic!("Invalid char");
                }
            } else {
                let mut u32char = linearray.len() as u32;
                // skip reserved range - U+D800 to U+DFFF
                // unicode code points in this range can't be converted to unicode scalars
                if u32char >= 55296 {
                    u32char += 2048;
                }

                // 1114111 is the biggest unicode scalar, so stop here
                if u32char == 1114111 {
                    panic!("max unicode scalar reached");
                }

                linearray.push(line.to_owned());
                linehash.insert(line.to_owned(), u32char);

                chars.push(char::from_u32(u32char).unwrap());
            }
        }
        chars
    }
}
