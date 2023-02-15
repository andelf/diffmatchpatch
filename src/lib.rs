use core::char;

use std::{collections::HashMap, iter::FromIterator, result::Result, time::Duration};

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

    pub fn is_delete(&self) -> bool {
        match self {
            Diff::Delete(_) => true,
            _ => false,
        }
    }

    pub fn is_insert(&self) -> bool {
        match self {
            Diff::Insert(_) => true,
            _ => false,
        }
    }

    pub fn is_equal(&self) -> bool {
        match self {
            Diff::Equal(_) => true,
            _ => false,
        }
    }

    /// If the text if empty, then it's a no-op
    pub fn is_nop(&self) -> bool {
        self.text().is_empty()
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
            if text1[pointerstart] == text2[pointerstart] {
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
            text2_trunc = text2;
        } else {
            text1_trunc = text1;
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
            let patern = &text1_trunc[(len - length)..len];
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
        self.diff_timeout?;

        let text1 = text1;
        let text2 = text2;

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
        let seed = Vec::from_iter(long_text[i..(i + long_len / 4)].iter().cloned());
        let mut best_common: &[char] = &[];
        let mut best_longtext_a: &[char] = &[];
        let mut best_longtext_b: &[char] = &[];
        let mut best_shorttext_a: &[char] = &[];
        let mut best_shorttext_b: &[char] = &[];
        let mut j = self.kmp(short_text, &seed, 0);
        while j.is_some() {
            let prefix_length = self.diff_common_prefix(&long_text[i..], &short_text[j.unwrap()..]);
            let suffix_length = self.diff_common_suffix(&long_text[..i], &short_text[..j.unwrap()]);
            if best_common.len() < suffix_length + prefix_length {
                best_common =
                    &short_text[(j.unwrap() - suffix_length)..(j.unwrap() + prefix_length)];
                best_longtext_a = &long_text[..(i - suffix_length)];
                best_longtext_b = &long_text[(i + prefix_length)..];
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

    /**
      Reorder and merge like edit sections.  Merge equalities.
      Any edit section can move as long as it doesn't cross an equality.

      Args:
          diffs: vectors of diff object.
    */
    pub fn diff_cleanup_merge(&self, diffs: &mut Vec<Diff>) {
        if diffs.is_empty() {
            return;
        }
        diffs.push(Diff::Equal("".into()));
        let mut text_insert = Chars::new();
        let mut text_delete = Chars::new();
        let mut i: usize = 0;
        let mut count_insert = 0;
        let mut count_delete = 0;
        while i < diffs.len() {
            if diffs[i].is_delete() {
                text_delete += diffs[i].text();
                count_delete += 1;
                i += 1;
            } else if diffs[i].is_insert() {
                text_insert += diffs[i].text();
                count_insert += 1;
                i += 1;
            } else {
                // equal
                // Upon reaching an equality, check for prior redundancies.
                if count_delete + count_insert > 1 {
                    if count_delete != 0 && count_insert != 0 {
                        // Factor out any common prefixies.
                        let mut commonlength = self.diff_common_prefix(&text_insert, &text_delete);
                        if commonlength != 0 {
                            let temp1 = &text_insert[..commonlength];
                            //  i - count_delete - count_insert - 1
                            let x = i.checked_sub(count_delete + count_insert + 1);

                            if x.is_some() && diffs[x.unwrap()].is_equal() {
                                *diffs[x.unwrap()].text_mut() += temp1;
                            } else {
                                diffs.insert(0, Diff::Equal(temp1.into()));
                                i += 1;
                            }
                            text_insert = text_insert[commonlength..].into();
                            text_delete = text_delete[commonlength..].into();
                        }

                        // Factor out any common suffixies.
                        commonlength = self.diff_common_suffix(&text_insert, &text_delete);
                        if commonlength != 0 {
                            let temp2 =
                                Chars::from(&text_insert[text_insert.len() - commonlength..])
                                    + diffs[i].text();
                            *diffs[i].text_mut() = temp2;
                            text_insert = text_insert[..text_insert.len() - commonlength].into();
                            text_delete = text_delete[..text_delete.len() - commonlength].into();
                        }
                    }

                    // Delete the offending records and add the merged ones.
                    i -= count_delete + count_insert;
                    for _j in 0..(count_delete + count_insert) {
                        diffs.remove(i);
                    }
                    if !text_delete.is_empty() {
                        diffs.insert(i, Diff::Delete(text_delete.clone()));
                        i += 1;
                    }
                    if !text_insert.is_empty() {
                        diffs.insert(i, Diff::Insert(text_insert.clone()));
                        i += 1;
                    }
                    i += 1;
                } else if i != 0 && diffs[i - 1].is_equal() {
                    // Merge this equality with the previous one.
                    // temp variable to avoid borrow checker
                    let temp1 = diffs[i - 1].text_mut().take() + diffs[i].text();
                    *diffs[i - 1].text_mut() = temp1;
                    diffs.remove(i);
                } else {
                    i += 1;
                }
                count_delete = 0;
                text_delete.clear();
                text_insert.clear();
                count_insert = 0;
            } // equal ends
        }
        // Remove the dummy entry at the end.
        if diffs[diffs.len() - 1].text().is_empty() {
            diffs.pop();
        }

        // Second pass: look for single edits surrounded on both sides by equalities
        // which can be shifted sideways to eliminate an equality.
        // e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
        let mut changes = false;
        i = 1;
        // Intentionally ignore the first and last element (don't need checking).
        while i < diffs.len() - 1 {
            if diffs[i - 1].is_equal() && diffs[i + 1].is_equal() {
                // This is a single edit surrounded by equalities.
                if diffs[i].text().ends_with(diffs[i - 1].text()) {
                    // Shift the edit over the previous equality.
                    //  A<ins>BA</ins>C -> <ins>AB</ins>AC
                    if !diffs[i - 1].text().is_empty() {
                        let prev = diffs[i - 1].text();
                        let next = diffs[i + 1].text();

                        // temp variables to eliminate borrow checker errors
                        let temp1 =
                            prev.to_owned() + diffs[i].text().slice_to(-(prev.len() as isize));
                        let temp2 = prev.to_owned() + next;
                        *diffs[i].text_mut() = temp1;
                        *diffs[i + 1].text_mut() = temp2;
                    }

                    diffs.remove(i - 1); // remove prev
                    changes = true;
                } else if diffs[i].text().starts_with(diffs[i + 1].text()) {
                    // Shift the edit over the next equality.
                    //  A<ins>CB</ins>C -> AC<ins>BC</ins>
                    let prev = diffs[i - 1].text();
                    let next = diffs[i + 1].text();

                    let temp1 = prev.to_owned() + next;
                    let temp2 = Chars::from(diffs[i].text()[next.len()..].to_owned()) + next;
                    *diffs[i - 1].text_mut() = temp1;
                    *diffs[i].text_mut() = temp2;

                    diffs.remove(i + 1);
                    changes = true;
                }
            }
            i += 1;
        }

        // If shifts were made, the diff needs reordering and another shift sweep.
        if changes {
            self.diff_cleanup_merge(diffs);
        }
    }

    /**
      Look for single edits surrounded on both sides by equalities
      which can be shifted sideways to align the edit to a word boundary.
      e.g: The c<ins>at c</ins>ame. -> The <ins>cat </ins>came.

      Args:
          diffs: Vector of diff object.
    */
    pub fn diff_cleanup_semantic_lossless(&self, diffs: &mut Vec<Diff>) {
        /**
        Given two strings, compute a score representing whether the
        internal boundary falls on logical boundaries.
        Scores range from 6 (best) to 0 (worst).
        Closure, but does not reference any external variables.

        Args:
            one: First chars.
            two: Second chars.

        Returns:
            The score.
        */
        fn diff_cleanup_semantic_score(one: &[char], two: &[char]) -> i32 {
            if one.is_empty() || two.is_empty() {
                // Edges are the best.
                return 6;
            }

            // Each port of this function behaves slightly differently due to
            // subtle differences in each language's definition of things like
            // 'whitespace'.  Since this function's purpose is largely cosmetic,
            // the choice has been made to use each language's native features
            // rather than force total conformity.
            let char1 = one[one.len() - 1];
            let char2 = two[0];
            let nonalphanumeric1: bool = !char1.is_alphanumeric();
            let nonalphanumeric2: bool = !char2.is_alphanumeric();
            let whitespace1: bool = nonalphanumeric1 & char1.is_whitespace();
            let whitespace2: bool = nonalphanumeric2 & char2.is_whitespace();
            let linebreak1: bool = whitespace1 & ((char1 == '\r') | (char1 == '\n'));
            let linebreak2: bool = whitespace2 & ((char2 == '\r') | (char2 == '\n'));

            let blanklineend1: bool =
                one.ends_with(&['\n', '\n']) || one.ends_with(&['\n', '\r', '\n']);
            let blanklinestart2: bool =
                two.starts_with(&['\n', '\n']) || two.starts_with(&['\r', '\n', '\r', '\n']);

            let blankline1: bool = linebreak1 & blanklineend1;
            let blankline2: bool = linebreak2 & blanklinestart2;

            if blankline1 || blankline2 {
                // Five points for blank lines.
                return 5;
            }
            if linebreak1 || linebreak2 {
                // Four points for line breaks.
                return 4;
            }
            if nonalphanumeric1 && !whitespace1 && whitespace2 {
                // Three points for end of sentences.
                return 3;
            }
            if whitespace1 || whitespace2 {
                // Two points for whitespace.
                return 2;
            }
            if nonalphanumeric1 || nonalphanumeric2 {
                // One point for non-alphanumeric.
                return 1;
            }
            0
        }

        let mut i = 1;
        if diffs.len() <= 1 {
            return;
        }

        // Intentionally ignore the first and last element (don't need checking).
        while i < diffs.len() - 1 {
            if diffs[i - 1].is_equal() && diffs[i + 1].is_equal() {
                // This is a single edit surrounded by equalities.
                // NOTE: sting concat operations, so use owned Chars type
                let mut equality1 = diffs[i - 1].text().clone();
                let mut edit = diffs[i].text().clone();
                let mut equality2 = diffs[i + 1].text().clone();

                // First, shift the edit as far left as possible.
                let common_offset = self.diff_common_suffix(diffs[i - 1].text(), diffs[i].text());
                if common_offset != 0 {
                    let common_string = edit.slice_from(-(common_offset as isize));
                    let temp1 =
                        Chars::from(common_string) + edit.slice_to(-(common_offset as isize));
                    let temp2 = Chars::from(common_string) + &equality2;
                    equality1 = Chars::from(equality1.slice_to(-(common_offset as isize)));
                    edit = temp1;
                    equality2 = temp2;
                }

                // Second, step character by character right, looking for the best fit.
                let mut best_equality1 = equality1.clone();
                let mut best_edit = edit.clone();
                let mut best_equality2 = equality2.clone();

                let mut best_score = diff_cleanup_semantic_score(&equality1, &edit)
                    + diff_cleanup_semantic_score(&edit, &equality2);

                while edit.len() > 0 && equality2.len() > 0 && edit[0] == equality2[0] {
                    // shift 1 char
                    equality1.push(edit[0]);
                    edit.remove(0);
                    edit.push(equality2[0]);
                    equality2.remove(0);

                    let score = diff_cleanup_semantic_score(&equality1, &edit)
                        + diff_cleanup_semantic_score(&edit, &equality2);

                    // The >= encourages trailing rather than leading whitespace on edits.
                    if score >= best_score {
                        best_score = score;
                        best_equality1 = equality1.clone();
                        best_edit = edit.clone();
                        best_equality2 = equality2.clone();
                    }
                }

                if diffs[i - 1].text() != &best_equality1 {
                    // We have an improvement, save it back to the diff.
                    if !best_equality1.is_empty() {
                        *diffs[i - 1].text_mut() = best_equality1;
                    } else {
                        diffs.remove(i - 1);
                        i -= 1;
                    }
                    *diffs[i].text_mut() = best_edit;
                    if !best_equality2.is_empty() {
                        *diffs[i + 1].text_mut() = best_equality2;
                    } else {
                        diffs.remove(i + 1);
                        i -= 1;
                    }
                }
            }
            i += 1;
        }
    }

    /**
      Reduce the number of edits by eliminating semantically trivial
      equalities.

      Args:
          diffs: Vectors of diff object.
    */
    pub fn diff_cleanup_semantic(&self, diffs: &mut Vec<Diff>) {
        let mut changes = false;
        let mut equalities: Vec<usize> = vec![]; // Stack of indices where equalities are found.
        let mut last_equality = Chars::new(); // Always equal to diffs[equalities[-1]][1]
        let mut i: usize = 0; // Index of current position.

        // Number of chars that changed prior to the equality.
        let mut length_insertions1 = 0;
        let mut length_deletions1 = 0;
        // Number of chars that changed after the equality.
        let mut length_insertions2 = 0;
        let mut length_deletions2 = 0;
        while i < diffs.len() {
            if diffs[i].is_equal() {
                // Equality found.
                equalities.push(i);
                length_insertions1 = length_insertions2;
                length_insertions2 = 0;
                length_deletions1 = length_deletions2;
                length_deletions2 = 0;
                last_equality = diffs[i].text().clone();
            } else {
                // An insertion or deletion.
                if diffs[i].is_insert() {
                    length_insertions2 += diffs[i].text().len();
                } else {
                    length_deletions2 += diffs[i].text().len();
                }
                // Eliminate an equality that is smaller or equal to the
                // edits on both sides of it.
                if !last_equality.is_empty()
                    && last_equality.len() <= usize::max(length_insertions1, length_deletions1)
                    && last_equality.len() <= usize::max(length_insertions2, length_deletions2)
                {
                    // Duplicate record.
                    diffs.insert(
                        equalities[equalities.len() - 1],
                        Diff::Delete(last_equality.clone()),
                    );
                    // Change second copy to insert.
                    diffs[equalities[equalities.len() - 1] + 1] =
                        Diff::Insert(diffs[equalities[equalities.len() - 1] + 1].text().into());
                    // Throw away the equality we just deleted.
                    equalities.pop();
                    // Throw away the previous equality (it needs to be reevaluated).
                    if !equalities.is_empty() {
                        equalities.pop();
                    }
                    // Reset the counters.
                    length_insertions1 = 0;
                    length_deletions1 = 0;
                    length_insertions2 = 0;
                    length_deletions2 = 0;
                    last_equality = Chars::new();
                    changes = true;

                    // NOT: reordered control flow, to use continue
                    if !equalities.is_empty() {
                        i = equalities[equalities.len() - 1];
                    } else {
                        i = 0;
                        continue;
                    }
                }
            }
            i += 1;
        }
        // Normalize the diff.
        if changes {
            self.diff_cleanup_merge(diffs);
        }
        self.diff_cleanup_semantic_lossless(diffs);

        // Find any overlaps between deletions and insertions.
        // e.g: <del>abcxxx</del><ins>xxxdef</ins>
        //   -> <del>abc</del>xxx<ins>def</ins>
        // e.g: <del>xxxabc</del><ins>defxxx</ins>
        //   -> <ins>def</ins>xxx<del>abc</del>
        // Only extract an overlap if it is as big as the edit ahead or behind it.
        i = 1;
        while i < diffs.len() {
            if diffs[i - 1].is_delete() && diffs[i].is_insert() {
                let deletion = diffs[i - 1].text().clone();
                let insertion = diffs[i].text().clone();
                let overlap_length1 = self.diff_common_overlap(&deletion, &insertion);
                let overlap_length2 = self.diff_common_overlap(&insertion, &deletion);
                if overlap_length1 >= overlap_length2 {
                    if (overlap_length1 as f32) >= (deletion.len() as f32 / 2.0)
                        || (overlap_length1 as f32) >= (insertion.len() as f32 / 2.0)
                    {
                        // Overlap found.  Insert an equality and trim the surrounding edits.
                        diffs.insert(i, Diff::Equal(insertion[..overlap_length1].into()));
                        diffs[i - 1] =
                            Diff::Delete(deletion[..deletion.len() - overlap_length1].into());
                        diffs[i + 1] = Diff::Insert(insertion[overlap_length1..].into());
                        i += 1;
                    }
                } else if (overlap_length2 as f32) >= (deletion.len() as f32 / 2.0)
                    || (overlap_length2 as f32) >= (insertion.len() as f32 / 2.0)
                {
                    // Reverse overlap found.
                    // Insert an equality and swap and trim the surrounding edits.
                    diffs.insert(i, Diff::Equal(deletion[..overlap_length2].into()));
                    // let insertion_vec_len = insertion_vec.len();
                    diffs[i - 1] =
                        Diff::Insert(insertion[..insertion.len() - overlap_length2].into());
                    diffs[i + 1] = Diff::Delete(deletion[overlap_length2..].into());
                    i += 1;
                }
                i += 1;
            }
            i += 1;
        }
    }

    /**
      Reduce the number of edits by eliminating operationally trivial
      equalities.

      Args:
          diffs: Vector of diff object.
    */
    pub fn diff_cleanup_efficiency(&mut self, diffs: &mut Vec<Diff>) {
        if diffs.is_empty() {
            return;
        }
        let mut changes: bool = false;
        let mut equalities: Vec<usize> = vec![]; // Stack of indices where equalities are found.
        let mut last_equality = Chars::new(); // Always equal to diffs[equalities[-1]][1]
        let mut i: usize = 0; // Index of current position.
        let mut pre_ins = false; // Is there an insertion operation before the last equality.
        let mut pre_del = false; // Is there a deletion operation before the last equality.
        let mut post_ins = false; // Is there an insertion operation after the last equality.
        let mut post_del = false; // Is there a deletion operation after the last equality.
        while i < diffs.len() {
            if diffs[i].is_equal() {
                if diffs[i].text().len() < self.edit_cost as usize && (post_del || post_ins) {
                    // Candidate found.
                    equalities.push(i);
                    pre_ins = post_ins;
                    pre_del = post_del;
                    last_equality = diffs[i].text().clone();
                } else {
                    // Not a candidate, and can never become one.
                    equalities = vec![];
                    // last_equality.clear();
                }
                post_ins = false;
                post_del = false;
            } else {
                // An insertion or deletion.
                if diffs[i].is_delete() {
                    post_del = true;
                } else {
                    post_ins = true;
                }

                /*
                Five types to be split:
                <ins>A</ins><del>B</del>XY<ins>C</ins><del>D</del>
                <ins>A</ins>X<ins>C</ins><del>D</del>
                <ins>A</ins><del>B</del>X<ins>C</ins>
                <ins>A</del>X<ins>C</ins><del>D</del>
                <ins>A</ins><del>B</del>X<del>C</del>
                */

                if !last_equality.is_empty()
                    && ((pre_ins && pre_del && post_del && post_ins)
                        || ((last_equality.len() as i32) < self.edit_cost / 2
                            && (pre_ins as i32
                                + pre_del as i32
                                + post_del as i32
                                + post_ins as i32)
                                == 3))
                {
                    // Duplicate record.
                    diffs.insert(
                        equalities[equalities.len() - 1],
                        Diff::Delete(last_equality.clone()),
                    );
                    // Change second copy to insert.
                    diffs[equalities[equalities.len() - 1] + 1] =
                        Diff::Insert(diffs[equalities[equalities.len() - 1] + 1].text().clone());
                    equalities.pop(); // Throw away the equality we just deleted.

                    last_equality.clear();
                    if pre_ins && pre_del {
                        // No changes made which could affect previous entry, keep going.
                        post_del = true;
                        post_ins = true;
                        equalities = vec![];
                    } else {
                        if !equalities.is_empty() {
                            equalities.pop(); // Throw away the previous equality.
                        }
                        if !equalities.is_empty() {
                            i = equalities[equalities.len() - 1];
                        } else {
                            i = 0;
                            // FIXME: reorder control flow
                            post_ins = false;
                            post_del = false;
                            changes = true;
                            continue;
                        }
                        post_ins = false;
                        post_del = false;
                    }
                    changes = true;
                }
            }
            i += 1;
        }
        if changes {
            self.diff_cleanup_merge(diffs);
        }
    }
}
