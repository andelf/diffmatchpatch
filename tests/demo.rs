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
