use std::{
    collections::HashMap,
    hash::Hash,
    time::{Duration, Instant},
};

pub use chars::Chars;
pub use patch::Patch;

pub mod chars;
mod r#match;
mod patch;

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

impl Default for DiffMatchPatch {
    fn default() -> Self {
        Self::new()
    }
}

/// The data structure representing a diff
#[derive(Debug, PartialEq, Clone)]
pub enum Diff<T = Chars> {
    Delete(T),
    Insert(T),
    Equal(T),
}

impl Diff {
    /// Placeholder for variable
    pub(crate) const fn empty() -> Self {
        Diff::Equal(Chars::new())
    }

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
        matches!(self, Diff::Delete(_))
    }

    pub fn is_insert(&self) -> bool {
        matches!(self, Diff::Insert(_))
    }

    pub fn is_equal(&self) -> bool {
        matches!(self, Diff::Equal(_))
    }

    pub fn translate<T: Clone>(&self, item_array: &[T]) -> Diff<Vec<T>> {
        match self {
            Diff::Delete(chars) => Diff::Delete(chars.translate(item_array)),
            Diff::Insert(chars) => Diff::Insert(chars.translate(item_array)),
            Diff::Equal(chars) => Diff::Equal(chars.translate(item_array)),
        }
    }
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
        let mut pattern: Vec<usize> = Vec::new();
        pattern.push(0);
        let mut len = 0;
        let mut i = 1;

        // Preprocess the pattern
        while i < len2 {
            if text2[i] == text2[len] {
                len += 1;
                pattern.push(len);
                i += 1;
            } else if len == 0 {
                pattern.push(0);
                i += 1;
            } else {
                len = pattern[len - 1];
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
                len = pattern[len - 1];
            }
        }
        None
    }

    /**
    Find the last index before a specific index in text1 where patern is present.

    Args:
        text1: Parent chars.
        text2: Patern chars.
        ind: index just before we have to find the patern.

    Returns:
        the last index where patern is found or -1 if not found.
    */
    fn rkmp(&mut self, text1: &[char], text2: &[char], ind: usize) -> Option<usize> {
        if text2.is_empty() {
            return Some(ind);
        }
        if text1.is_empty() {
            return None;
        }
        let len2 = text2.len();
        let mut pattern: Vec<usize> = Vec::new();
        pattern.push(0);
        let mut len = 0;
        let mut i = 1;

        // Preprocess the pattern
        while i < len2 {
            if text2[i] == text2[len] {
                len += 1;
                pattern.push(len);
                i += 1;
            } else if len == 0 {
                pattern.push(0);
                i += 1;
            } else {
                len = pattern[len - 1];
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
                    len = pattern[len - 1];
                }
            } else if len == 0 {
                i += 1;
            } else {
                len = pattern[len - 1];
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
    pub fn diff_chars_to_lines(&self, diffs: &mut [Diff], line_array: &[Chars]) {
        for diff in diffs.iter_mut() {
            let mut text = Chars::new();
            let text1 = diff.text();
            for j in 0..text1.len() {
                text += &line_array[text1[j] as usize];
            }
            *diff.text_mut() = text;
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
    pub fn diff_common_prefix(&self, text1: &[char], text2: &[char]) -> usize {
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
    pub fn diff_common_suffix(&self, text1: &[char], text2: &[char]) -> usize {
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
    pub fn diff_common_overlap(&self, text1: &[char], text2: &[char]) -> usize {
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
    pub fn diff_half_match<'a>(
        &self,
        text1: &'a [char],
        text2: &'a [char],
    ) -> Option<Vec<&'a [char]>> {
        self.diff_timeout?;

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
        let mut temp2 = hm[0];
        let mut temp3 = hm[2];
        hm[0] = temp3;
        hm[2] = temp2;
        temp2 = hm[1];
        temp3 = hm[3];
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
    fn diff_half_matchi<'a>(
        &self,
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

    pub fn diff_chars_to_any<T>(&self, diffs: &[Diff], item_array: &[T]) -> Vec<Diff<Vec<T>>>
    where
        T: Clone,
    {
        let mut result = Vec::with_capacity(diffs.len());
        for diff in diffs {
            result.push(diff.translate(item_array))
        }
        result
    }

    /// Reduce the sequences to a string
    // of hashes where each Unicode character represents one item.
    pub fn diff_any_to_chars<T>(&self, seq1: &[T], seq2: &[T]) -> (Chars, Chars, Vec<T>)
    where
        T: Hash + Eq + Clone + Default,
    {
        let mut itemarray: Vec<T> = vec![T::default()];
        let mut itemhash: HashMap<T, u32> = HashMap::new();
        let chars1 = self.diff_any_to_chars_munge(seq1, &mut itemarray, &mut itemhash);
        let chars2 = self.diff_any_to_chars_munge(seq2, &mut itemarray, &mut itemhash);
        (chars1, chars2, itemarray)
    }

    fn diff_any_to_chars_munge<T>(
        &self,
        seq: &[T],
        itemarray: &mut Vec<T>,
        itemhash: &mut HashMap<T, u32>,
    ) -> Chars
    where
        T: Hash + Eq + Clone + Default,
    {
        let mut chars = Chars::new();
        for item in seq {
            if let Some(ch) = itemhash.get(item) {
                if let Some(ch) = char::from_u32(*ch) {
                    chars.push(ch);
                } else {
                    panic!("Invalid char");
                }
            } else {
                let mut u32char = itemarray.len() as u32;
                // skip reserved range - U+D800 to U+DFFF
                // unicode code points in this range can't be converted to unicode scalars
                if u32char >= 55296 {
                    u32char += 2048;
                }

                // 1114111 is the biggest unicode scalar, so stop here
                if u32char == 1114111 {
                    panic!("max unicode scalar reached");
                }

                itemarray.push(item.clone());
                itemhash.insert(item.clone(), u32char);

                chars.push(char::from_u32(u32char).unwrap());
            }
        }
        chars
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
    pub fn diff_lines_to_chars(
        &self,
        text1: &[char],
        text2: &[char],
    ) -> (Chars, Chars, Vec<Chars>) {
        let mut linearray: Vec<Chars> = vec![Chars::new()];
        let mut linehash: HashMap<Chars, u32> = HashMap::new();
        let chars1 = self.diff_lines_to_chars_munge(text1, &mut linearray, &mut linehash);
        let chars2 = self.diff_lines_to_chars_munge(text2, &mut linearray, &mut linehash);
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
    fn diff_lines_to_chars_munge<'a>(
        &self,
        text: &[char],
        linearray: &'a mut Vec<Chars>,
        linehash: &'a mut HashMap<Chars, u32>,
    ) -> Chars {
        // Rust char type:
        // 0..<d800
        // d800..<e000
        // e000..<110000

        let mut chars = Chars::new();
        // Walk the text, pulling out a substring for each line.
        // text.split('\n') would would temporarily double our memory footprint.
        // Modifying text would create many large strings to garbage collect.
        for line in text.split_inclusive(|&c| c == '\n') {
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

                linearray.push(line.into());
                linehash.insert(line.into(), u32char);

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
        diffs.push(Diff::empty());
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
    pub fn diff_cleanup_efficiency(&self, diffs: &mut Vec<Diff>) {
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

    /**
      Compute and return the source text (all equalities and deletions).

      Args:
          diffs: Vectoe of diff object.

      Returns:
          Source text.
    */
    pub fn diff_text1(&self, diffs: &[Diff]) -> Chars {
        let mut text = Chars::new();
        for d in diffs {
            if !d.is_insert() {
                text += d.text();
            }
        }
        text
    }

    /**
      Compute and return the destination text (all equalities and insertions).

      Args:
          diffs: Vector of diff object.

      Returns:
          destination text.
    */
    pub fn diff_text2(&self, diffs: &[Diff]) -> Chars {
        let mut text = Chars::new();
        for d in diffs {
            if !d.is_delete() {
                text += d.text();
            }
        }
        text
    }

    /**
    loc is a location in text1, compute and return the equivalent location
    in text2.  e.g. "The cat" vs "The big cat", 1->1, 5->8

    Args:
        diffs: Vector of diff object.
        loc: Location within text1.

    Returns:
        Location within text2.
    */
    pub fn diff_xindex(&self, diffs: &[Diff], loc: usize) -> usize {
        let mut chars1 = 0;
        let mut chars2 = 0;
        let mut last_chars1 = 0;
        let mut last_chars2 = 0;
        let mut lastdiff = Diff::empty();
        let z = 0;
        for diffs_item in diffs {
            if !diffs_item.is_insert() {
                // Equality or deletion.
                chars1 += diffs_item.text().len();
            }
            if !diffs_item.is_delete() {
                // Equality or insertion.
                chars2 += diffs_item.text().len();
            }
            if chars1 > loc {
                // Overshot the location.
                lastdiff = diffs_item.clone();
                break;
            }
            last_chars1 = chars1;
            last_chars2 = chars2;
        }
        if lastdiff.is_delete() && diffs.len() != z {
            // The location was deleted.
            return last_chars2;
        }
        // Add the remaining len(character).
        last_chars2 + (loc - last_chars1)
    }

    /*
      Compute the Levenshtein distance; the number of inserted, deleted or
      substituted characters.

      Args:
          diffs: Vector of diff object.

      Returns:
          Number of changes.
    */
    pub fn diff_levenshtein(&self, diffs: &[Diff]) -> usize {
        let mut levenshtein = 0;
        let mut insertions = 0;
        let mut deletions = 0;
        for adiff in diffs {
            if adiff.is_insert() {
                insertions += adiff.text().len();
            } else if adiff.is_delete() {
                deletions += adiff.text().len();
            } else {
                // A deletion and an insertion is one substitution.
                levenshtein += usize::max(insertions, deletions);
                insertions = 0;
                deletions = 0;
            }
        }
        levenshtein += usize::max(insertions, deletions);
        levenshtein
    }

    pub fn diff_to_delta(&self, diffs: &[Diff]) -> String {
        diffs
            .iter()
            .map(|d| match d {
                Diff::Insert(text) => {
                    format!("+{}", text.to_safe_encode())
                }
                Diff::Delete(text) => {
                    format!("-{}", text.len())
                }
                Diff::Equal(text) => {
                    format!("={}", text.len())
                }
            })
            .collect::<Vec<_>>()
            .join("\t")
    }

    pub fn diff_from_delta(&self, _text1: &mut Chars, _delta: &str) {
        unimplemented!()
    }

    /**
    Find the 'middle snake' of a diff, split the problem in two
    and return the recursively constructed diff.
    See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.

    Args:
        text1: Old chars to be diffed.
        text2: New chars to be diffed.

    Returns:
            Vector of diffs as changes.
    */
    pub fn diff_bisect(&mut self, text1: &[char], text2: &[char]) -> Vec<Diff> {
        self.diff_bisect_internal(text1, text2, Instant::now())
    }

    fn diff_bisect_internal(
        &self,
        text1: &[char],
        text2: &[char],
        start_time: Instant,
    ) -> Vec<Diff> {
        let text1_length = text1.len() as i32;
        let text2_length = text2.len() as i32;
        let max_d: i32 = (text1_length + text2_length + 1) / 2;
        let v_offset: i32 = max_d;
        let v_length: i32 = 2 * max_d;
        let mut v1: Vec<i32> = vec![-1; v_length as usize];
        let mut v2: Vec<i32> = vec![-1; v_length as usize];
        v1[v_offset as usize + 1] = 0;
        v2[v_offset as usize + 1] = 0;
        let delta: i32 = text1_length - text2_length;
        // If the total number of characters is odd, then the front path will
        // collide with the reverse path.
        let front: i32 = (delta % 2 != 0) as i32;
        // Offsets for start and end of k loop.
        // Prevents mapping of space beyond the grid.
        let mut k1start: i32 = 0;
        let mut k1end: i32 = 0;
        let mut k2start: i32 = 0;
        let mut k2end: i32 = 0;
        for d in 0..max_d {
            if self.diff_timeout.is_some()
                && start_time.elapsed() >= *self.diff_timeout.as_ref().unwrap()
            {
                break;
            }

            let d1 = d;
            let mut k1 = -d1 + k1start;
            let mut x1: i32;
            let mut k1_offset: i32;
            let mut k2_offset;
            let mut x2;
            let mut y1;
            // Walk the front path one step.
            while k1 < d1 + 1 - k1end {
                k1_offset = v_offset + k1;
                if k1 == -d1
                    || (k1 != d1 && v1[k1_offset as usize - 1] < v1[k1_offset as usize + 1])
                {
                    x1 = v1[k1_offset as usize + 1];
                } else {
                    x1 = v1[k1_offset as usize - 1] + 1;
                }
                y1 = x1 - k1;
                while x1 < text1_length && y1 < text2_length {
                    let i1 = if x1 < 0 {
                        text1_length + x1
                    } else {
                        x1
                    };
                    let i2 = if y1 < 0 {
                        text2_length + y1
                    } else {
                        y1
                    };
                    if text1[i1 as usize] != text2[i2 as usize] {
                        break;
                    }
                    x1 += 1;
                    y1 += 1;
                }
                v1[k1_offset as usize] = x1;
                if x1 > text1_length {
                    // Ran off the right of the graph.
                    k1end += 2;
                } else if y1 > text2_length {
                    // Ran off the bottom of the graph.
                    k1start += 2;
                } else if front != 0 {
                    k2_offset = v_offset + delta - k1;
                    if k2_offset >= 0 && k2_offset < v_length && v2[k2_offset as usize] != -1 {
                        // Mirror x2 onto top-left coordinate system.
                        x2 = text1_length - v2[k2_offset as usize];
                        if x1 >= x2 {
                            // Overlap detected.
                            return self.diff_bisect_split(
                                text1,
                                text2,
                                x1 as usize,
                                y1 as usize,
                                start_time,
                            );
                        }
                    }
                }
                k1 += 2;
            }
            let mut k2 = -d1 + k2start;
            let mut y2;
            // Walk the reverse path one step.
            while k2 < d1 + 1 - k2end {
                k2_offset = v_offset + k2;
                if k2 == -d1
                    || (k2 != d1 && v2[k2_offset as usize - 1] < v2[k2_offset as usize + 1])
                {
                    x2 = v2[k2_offset as usize + 1];
                } else {
                    x2 = v2[k2_offset as usize - 1] + 1;
                }
                y2 = x2 - k2;
                while x2 < text1_length && y2 < text2_length {
                    let i1 = if text1_length - x2 > 0 {
                        text1_length - x2 - 1
                    } else {
                        x2 + 1
                    };
                    let i2 = if text2_length - y2 > 0 {
                        text2_length - y2 - 1
                    } else {
                        y2 + 1
                    };
                    if text1[i1 as usize] != text2[i2 as usize] {
                        break;
                    }
                    x2 += 1;
                    y2 += 1;
                }
                v2[k2_offset as usize] = x2;
                if x2 > text1_length {
                    // Ran off the left of the graph.
                    k2end += 2;
                } else if y2 > text2_length {
                    // Ran off the top of the graph.
                    k2start += 2;
                } else if front == 0 {
                    k1_offset = v_offset + delta - k2;
                    if k1_offset >= 0 && k1_offset < v_length && v1[k1_offset as usize] != -1 {
                        x1 = v1[k1_offset as usize];
                        y1 = v_offset + x1 - k1_offset;
                        // Mirror x2 onto top-left coordinate system.
                        x2 = text1_length - x2;
                        if x1 >= x2 {
                            // Overlap detected.
                            return self.diff_bisect_split(
                                text1,
                                text2,
                                x1 as usize,
                                y1 as usize,
                                start_time,
                            );
                        }
                    }
                }
                k2 += 2;
            }
        }
        // number of diffs equals number of characters, no commonality at all.
        vec![Diff::Delete(text1.into()), Diff::Insert(text2.into())]
    }

    /**
    Given the location of the 'middle snake', split the diff in two parts
    and recurse.

    Args:
        text1: Old text1 to be diffed.
        text2: New text1 to be diffed.
        x: Index of split point in text1.
        y: Index of split point in text2.

    Returns:
            Vector of diffs as changes.
    */
    fn diff_bisect_split(
        &self,
        text1: &[char],
        text2: &[char],
        x: usize,
        y: usize,
        start_time: Instant,
    ) -> Vec<Diff> {
        let text1a = &text1[..x];
        let text2a = &text2[..y];
        let text1b = &text1[x..];
        let text2b = &text2[y..];

        // Compute both diffs serially.
        let mut diffs = self.diff_main_internal(text1a, text2a, false, start_time);
        let mut diffsb = self.diff_main_internal(text1b, text2b, false, start_time);
        diffs.append(&mut diffsb);
        diffs
    }

    /**
    Find the differences between two texts.  Simplifies the problem by
      stripping any common prefix or suffix off the texts before diffing.

    Args:
        text1: Old string to be diffed.
        text2: New string to be diffed.
        checklines: Optional speedup flag. If present and false, then don't run
            a line-level diff first to identify the changed areas.
            Defaults to true, which does a faster, slightly less optimal diff.
    Returns:
        Vector of diffs as changes.
    */
    pub fn diff_main(&self, text1: &[char], text2: &[char], checklines: bool) -> Vec<Diff> {
        self.diff_main_internal(text1, text2, checklines, Instant::now())
    }

    fn diff_main_internal(
        &self,
        text1: &[char],
        text2: &[char],
        checklines: bool,
        start_time: Instant,
    ) -> Vec<Diff> {
        let mut text1 = text1;
        let mut text2 = text2;
        // check for empty text
        if text1.is_empty() && text2.is_empty() {
            return vec![];
        } else if text1.is_empty() {
            return vec![Diff::Insert(text2.into())];
        } else if text2.is_empty() {
            return vec![Diff::Delete(text1.into())];
        }

        // check for equality
        if text1 == text2 {
            return vec![Diff::Equal(text1.into())];
        }

        // Trim off common prefix (speedup).
        let mut commonlength = self.diff_common_prefix(text1, text2);
        let commonprefix = &text1[0..commonlength];
        text1 = &text1[commonlength..];
        text2 = &text2[commonlength..];

        // Trim off common suffix (speedup).
        commonlength = self.diff_common_suffix(text1, text2);
        let commonsuffix = &text1[(text1.len() - commonlength)..];
        text1 = &text1[..(text1.len() - commonlength)];
        text2 = &text2[..(text2.len() - commonlength)];

        let mut diffs: Vec<Diff> = Vec::new();

        //Restore the prefix
        if !commonprefix.is_empty() {
            diffs.push(Diff::Equal(commonprefix.into()));
        }

        // Compute the diff on the middle block.
        let temp = self.diff_compute(text1, text2, checklines, start_time);
        for z in temp {
            diffs.push(z);
        }

        // Restore the suffix
        if !commonsuffix.is_empty() {
            diffs.push(Diff::Equal(commonsuffix.into()));
        }
        self.diff_cleanup_merge(&mut diffs);
        diffs
    }

    /**
    Find the differences between two texts.  Assumes that the texts do not
    have any common prefix or suffix.

    Args:
        text1: Old chars to be diffed.
        text2: New chars to be diffed.
        checklines: Speedup flag.  If false, then don't run a line-level diff
        first to identify the changed areas.
        If true, then run a faster, slightly less optimal diff.

    Returns:
        Vector of diffs as changes.
    */
    fn diff_compute(
        &self,
        text1: &[char],
        text2: &[char],
        checklines: bool,
        start_time: Instant,
    ) -> Vec<Diff> {
        let mut diffs: Vec<Diff> = Vec::new();
        if text1.is_empty() {
            // Just add some text (speedup).
            diffs.push(Diff::Insert(text2.into()));
            return diffs;
        } else if text2.is_empty() {
            // Just delete some text (speedup).
            diffs.push(Diff::Delete(text1.into()));
            return diffs;
        }
        {
            let len1 = text1.len();
            let len2 = text2.len();
            let (longtext, shorttext) = if len1 >= len2 {
                (text1, text2)
            } else {
                (text2, text1)
            };
            if let Some(i) = self.kmp(longtext, shorttext, 0) {
                // Shorter text is inside the longer text (speedup).
                if len1 > len2 {
                    if i != 0 {
                        diffs.push(Diff::Delete(text1[..i].into()));
                    }
                    diffs.push(Diff::Equal(text2.into()));
                    if i + text2.len() != text1.len() {
                        diffs.push(Diff::Delete(text1[i + text2.len()..].into()));
                    }
                } else {
                    if i != 0 {
                        diffs.push(Diff::Insert(text2[..i].into()));
                    }
                    diffs.push(Diff::Equal(text1.into()));
                    if i + text1.len() != text2.len() {
                        diffs.push(Diff::Insert(text2[i + text1.len()..].into()));
                    }
                }
                return diffs;
            }
            if shorttext.len() == 1 {
                // Single character string.
                // After the previous speedup, the character can't be an equality.
                diffs.push(Diff::Delete(text1.into()));
                diffs.push(Diff::Insert(text2.into()));
                return diffs;
            }
        }
        // Check to see if the problem can be split in two.
        let hm = self.diff_half_match(text1, text2);
        if let Some(hm) = hm {
            // A half-match was found, sort out the return data.
            match hm[..] {
                [text1_a, text1_b, text2_a, text2_b, mid_common] => {
                    // Send both pairs off for separate processing.
                    let mut diffs_a =
                        self.diff_main_internal(text1_a, text2_a, checklines, start_time);
                    let diffs_b = self.diff_main_internal(text1_b, text2_b, checklines, start_time);
                    // Merge the result.
                    diffs_a.push(Diff::Equal(mid_common.into()));
                    diffs_a.extend(diffs_b);
                    return diffs_a;
                }
                _ => unreachable!("vec used as 5-tuple"),
            }
        }

        if checklines && text1.len() > 100 && text2.len() > 100 {
            self.diff_linemode_internal(text1, text2, start_time)
        } else {
            self.diff_bisect_internal(text1, text2, start_time)
        }
    }

    /**
    Do a quick line-level diff on both chars, then rediff the parts for
    greater accuracy.
    This speedup can produce non-minimal diffs.

    Args:
        text1: Old chars to be diffed.
        text2: New chars to be diffed.

    Returns:
        Vector of diffs as changes.
    */
    pub fn diff_linemode(&mut self, text1: &[char], text2: &[char]) -> Vec<Diff> {
        self.diff_linemode_internal(text1, text2, Instant::now())
    }

    fn diff_linemode_internal(
        &self,
        text1: &[char],
        text2: &[char],
        start_time: Instant,
    ) -> Vec<Diff> {
        // Scan the text on a line-by-line basis first.
        let (text3, text4, linearray) = self.diff_lines_to_chars(text1, text2);

        let dmp = DiffMatchPatch::new();
        let mut diffs: Vec<Diff> = dmp.diff_main_internal(&text3, &text4, false, start_time);

        // Convert the diff back to original text.
        self.diff_chars_to_lines(&mut diffs, &linearray);
        // Eliminate freak matches (e.g. blank lines)
        self.diff_cleanup_semantic(&mut diffs);

        // Rediff any replacement blocks, this time character-by-character.
        // Add a dummy entry at the end.
        diffs.push(Diff::empty());
        let mut count_delete = 0;
        let mut count_insert = 0;
        let mut text_delete = Chars::new();
        let mut text_insert = Chars::new();
        let mut i = 0;
        let mut temp: Vec<Diff> = vec![];
        while i < diffs.len() {
            if diffs[i].is_insert() {
                count_insert += 1;
                text_insert += diffs[i].text();
            } else if diffs[i].is_delete() {
                count_delete += 1;
                text_delete += diffs[i].text();
            } else {
                // Upon reaching an equality, check for prior redundancies.
                if count_delete >= 1 && count_insert >= 1 {
                    // Delete the offending records and add the merged ones.
                    let sub_diff =
                        self.diff_main_internal(&text_delete, &text_insert, false, start_time);
                    for z in sub_diff {
                        temp.push(z);
                    }
                    temp.push(diffs[i].clone());
                } else {
                    if !text_delete.is_empty() {
                        temp.push(Diff::Delete(text_delete));
                    }
                    if !text_insert.is_empty() {
                        temp.push(Diff::Insert(text_insert));
                    }
                    temp.push(diffs[i].clone());
                }
                count_delete = 0;
                count_insert = 0;
                text_delete = Chars::new();
                text_insert = Chars::new();
            }
            i += 1;
        }
        temp.pop(); //Remove the dummy entry at the end.
        temp
    }

    // unimplemented:
    // DiffPrettyHtml
    // DiffToDelta
}
