use std::{hash::Hash, time::Duration};

pub use crate::{Chars, Diff, DiffMatchPatch, ToChars};

/// Raw diff
pub fn diff_main(text1: &str, text2: &str) -> Vec<Diff<String>> {
    let dmp = DiffMatchPatch::new();

    let text1 = text1.to_chars();
    let text2 = text2.to_chars();
    let diffs = dmp.diff_main(&text2, &text1, false);

    diffs.into_iter().map(|d| d.translate(|text| text.to_string())).collect()
}

/// Semantic Cleanup
pub fn diff_semantic(text1: &str, text2: &str) -> Vec<Diff<String>> {
    let dmp = DiffMatchPatch::new();

    let text1 = text1.to_chars();
    let text2 = text2.to_chars();
    let mut diffs = dmp.diff_main(&text2, &text1, false);

    dmp.diff_cleanup_semantic(&mut diffs);

    diffs.into_iter().map(|d| d.translate(|text| text.to_string())).collect()
}

/// Diff in line mode
pub fn diff_line_mode(text1: &str, text2: &str) -> Vec<Diff<String>> {
    let dmp = DiffMatchPatch::new();

    let words1: Vec<&str> = text1.split_inclusive('\n').collect();
    let words2: Vec<&str> = text2.split_inclusive('\n').collect();

    let (chars1, chars2, word_array) = dmp.diff_any_to_chars(&words1, &words2);
    let diffs = dmp.diff_main(&chars1, &chars2, false);
    let new_diffs = dmp.diff_chars_to_any(&diffs, &word_array);

    new_diffs.into_iter().map(|d| d.translate(|text| text.concat())).collect()
}

/// Diff in word mode, only for demo
pub fn diff_word_mode(text1: &str, text2: &str) -> Vec<Diff<String>> {
    let dmp = DiffMatchPatch::new();

    let words1: Vec<&str> =
        text1.split_inclusive(|c: char| c.is_whitespace() || c == ',' || c == '.').collect();
    let words2: Vec<&str> =
        text2.split_inclusive(|c: char| c.is_whitespace() || c == ',' || c == '.').collect();

    let (chars1, chars2, word_array) = dmp.diff_any_to_chars(&words1, &words2);
    let diffs = dmp.diff_main(&chars1, &chars2, false);
    let new_diffs = dmp.diff_chars_to_any(&diffs, &word_array);

    new_diffs.into_iter().map(|d| d.translate(|text| text.concat())).collect()
}

/// Diff any sequence
pub fn diff_any<T>(text1: &[T], text2: &[T]) -> Vec<Diff<Vec<T>>>
where
    T: Eq + Copy + Hash + Default,
{
    let dmp = DiffMatchPatch::new();

    let (chars1, chars2, word_array) = dmp.diff_any_to_chars(text1, text2);
    let diffs = dmp.diff_main(&chars1, &chars2, false);
    dmp.diff_chars_to_any(&diffs, &word_array)
}

/// Text differ
pub struct Differ {
    dmp: DiffMatchPatch,
    do_cleanup_semantic: bool,
    do_cleanup_efficiency: bool,
}

impl Default for Differ {
    fn default() -> Self {
        Self::new()
    }
}

impl Differ {
    pub fn new() -> Self {
        Differ {
            dmp: DiffMatchPatch::new(),
            do_cleanup_semantic: false,
            do_cleanup_efficiency: false,
        }
    }

    /// Set timeout for diff operation
    pub fn timeout(&mut self, timeout: Option<Duration>) -> &mut Self {
        self.dmp.diff_timeout = timeout;
        self
    }

    /// Do semantic cleanup after diff
    pub fn cleanup_semantic(&mut self) -> &mut Self {
        self.do_cleanup_semantic = true;
        self
    }

    /// Do efficiency cleanup after diff
    pub fn cleanup_efficiency(&mut self, edit_cost: i32) -> &mut Self {
        self.do_cleanup_efficiency = true;
        self.dmp.edit_cost = edit_cost;
        self
    }

    /// Diff two texts
    pub fn diff(&self, text1: &str, text2: &str) -> Vec<Diff<String>> {
        let text1 = text1.to_chars();
        let text2 = text2.to_chars();
        let mut diffs = self.dmp.diff_main(&text2, &text1, false);

        if self.do_cleanup_semantic {
            self.dmp.diff_cleanup_semantic(&mut diffs);
        }

        if self.do_cleanup_efficiency {
            self.dmp.diff_cleanup_efficiency(&mut diffs);
        }

        diffs.into_iter().map(|d| d.translate(|text| text.to_string())).collect()
    }

    /// Diff two texts represented by chars
    pub fn diff_chars(&self, text1: &[char], text2: &[char]) -> Vec<Diff<Chars>> {
        let mut diffs = self.dmp.diff_main(text2, text1, false);

        if self.do_cleanup_semantic {
            self.dmp.diff_cleanup_semantic(&mut diffs);
        }

        if self.do_cleanup_efficiency {
            self.dmp.diff_cleanup_efficiency(&mut diffs);
        }

        diffs
    }
}
