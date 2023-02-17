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
