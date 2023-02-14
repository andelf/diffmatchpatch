#[cfg(test)]
use diffmatchpatch::*;

#[test]
fn test_diff_common_prefix() {
    let dmp = DiffMatchPatch::new();

    assert_eq!(0, dmp.diff_common_prefix("abc".to_chars(), "xyz".to_chars()));
    assert_eq!(4, dmp.diff_common_prefix("1234abcdef".to_chars(), "1234xyz".to_chars()));
    assert_eq!(4, dmp.diff_common_prefix("1234".to_chars(), "1234xyz".to_chars()));
}

#[test]
fn test_diff_common_suffix() {
    let dmp = DiffMatchPatch::new();

    assert_eq!(0, dmp.diff_common_suffix("abc".to_chars(), "xyz".to_chars()));
    assert_eq!(4, dmp.diff_common_suffix("abcdef1234".to_chars(), "xyz1234".to_chars()));
    assert_eq!(4, dmp.diff_common_suffix("1234".to_chars(), "xyz1234".to_chars()));
}

#[test]
fn test_diff_common_overlap() {
    let dmp = DiffMatchPatch::new();

    assert_eq!(0, dmp.diff_common_overlap("".to_chars(), "abcd".to_chars()), "null case");
    assert_eq!(3, dmp.diff_common_overlap("abc".to_chars(), "abcd".to_chars()), "whole case");
    assert_eq!(0, dmp.diff_common_overlap("123456".to_chars(), "abcd".to_chars()), "no overlap");
    assert_eq!(3, dmp.diff_common_overlap("123456xxx".to_chars(), "xxxabcd".to_chars()), "overlap");
    // Some overly clever languages (C#) may treat ligatures as equal to their
    // component letters.  E.g. U+FB01 == 'fi'
    assert_eq!(0, dmp.diff_common_overlap("fi".to_chars(), "\u{fb01}i".to_chars()), "unicode");

}
