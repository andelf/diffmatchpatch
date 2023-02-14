use std::time::Duration;

#[cfg(test)]
use diffmatchpatch::*;

pub trait ToCharsVec {
    fn to_chars_vec(&self) -> Vec<Vec<char>>;
}

impl ToCharsVec for Vec<&str> {
    fn to_chars_vec(&self) -> Vec<Vec<char>> {
        self.iter().map(|s| s.to_chars()).collect()
    }
}

impl ToCharsVec for [&str; 5] {
    fn to_chars_vec(&self) -> Vec<Vec<char>> {
        self.iter().map(|s| s.to_chars()).collect()
    }
}

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

#[test]
fn test_diff_half_match() {
    // Detect a halfmatch
    let mut dmp = DiffMatchPatch::new();
    dmp.diff_timeout = Some(Duration::from_secs(1));

    // No match
    assert_eq!(None, dmp.diff_half_match(&"1234567890".to_chars(), &"abcdef".to_chars()));
    assert_eq!(None, dmp.diff_half_match(&"12345".to_chars(), &"23".to_chars()));
    // Single Match

    let text1 = "1234567890".to_chars();
    let text2 = "a345678z".to_chars();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(
        Some(vec![
            &*"12".to_chars(),
            &*"90".to_chars(),
            &*"a".to_chars(),
            &*"z".to_chars(),
            &*"345678".to_chars()
        ]),
        matches
    );

    let text1 = "a345678z".to_chars();
    let text2 = "1234567890".to_chars();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(
        Some(vec![
            &*"a".to_chars(),
            &*"z".to_chars(),
            &*"12".to_chars(),
            &*"90".to_chars(),
            &*"345678".to_chars()
        ]),
        matches
    );

    let text1 = "abc56789z".to_chars();
    let text2 = "1234567890".to_chars();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(
        Some(vec![
            &*"abc".to_chars(),
            &*"z".to_chars(),
            &*"1234".to_chars(),
            &*"0".to_chars(),
            &*"56789".to_chars()
        ]),
        matches
    );

    let text1 = "a23456xyz".to_chars();
    let text2 = "1234567890".to_chars();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(
        Some(vec![
            &*"a".to_chars(),
            &*"xyz".to_chars(),
            &*"1".to_chars(),
            &*"7890".to_chars(),
            &*"23456".to_chars()
        ]),
        matches
    );

    // Multiple Matches.
    let text1 = "121231234123451234123121".to_chars();
    let text2 = "a1234123451234z".to_chars();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(
        Some(vec![
            &*"12123".to_chars(),
            &*"123121".to_chars(),
            &*"a".to_chars(),
            &*"z".to_chars(),
            &*"1234123451234".to_chars()
        ]),
        matches
    );

    let text1 = "x-=-=-=-=-=-=-=-=-=-=-=-=".to_chars();
    let text2 = "xx-=-=-=-=-=-=-=".to_chars();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(
        Some(vec![
            &*"".to_chars(),
            &*"-=-=-=-=-=".to_chars(),
            &*"x".to_chars(),
            &*"".to_chars(),
            &*"x-=-=-=-=-=-=-=".to_chars()
        ]),
        matches
    );

    let text1 = "-=-=-=-=-=-=-=-=-=-=-=-=y".to_chars();
    let text2 = "-=-=-=-=-=-=-=yy".to_chars();
    let ret: Vec<_> = ["-=-=-=-=-=", "", "", "y", "-=-=-=-=-=-=-=y"].to_chars_vec();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(Some(ret.iter().map(|s| &**s).collect::<Vec<_>>()), matches);

    // Non-optimal halfmatch.
    // Optimal diff would be -q+x=H-i+e=lloHe+Hu=llo-Hew+y not
    // -qHillo+x=HelloHe-w+Hulloy
    let text1 = "qHilloHelloHew".to_chars();
    let text2 = "xHelloHeHulloy".to_chars();
    let ret: Vec<_> = ["qHillo", "w", "x", "Hulloy", "HelloHe"].to_chars_vec();
    let matches = dmp.diff_half_match(&text1, &text2);
    assert_eq!(Some(ret.iter().map(|s| &**s).collect::<Vec<_>>()), matches);

    // Optimal no halfmatch
    dmp.diff_timeout = None;
    assert_eq!(
        None,
        dmp.diff_half_match(&"qHilloHelloHew".to_chars(), &"xHelloHeHulloy".to_chars())
    );
}

#[test]
pub fn test_diff_lines_to_chars() {
    let dmp = DiffMatchPatch::new();

    const CASES: &[(&str, &str, &str, &str, &[&str])] = &[
        ("a", "b", "\x01", "\x02", &["", "a", "b"]),
        (
            "alpha\nbeta\nalpha\n",
            "beta\nalpha\nbeta\n",
            "\x01\x02\x01",
            "\x02\x01\x02",
            &["", "alpha\n", "beta\n"],
        ),
        (
            "",
            "alpha\r\nbeta\r\n\r\n\r\n",
            "",
            "\x01\x02\x03\x03",
            &["", "alpha\r\n", "beta\r\n", "\r\n"],
        ),
        ("a", "b", "\x01", "\x02", &["", "a", "b"]),
    ];

    for (t1, t2, e1, e2, arr) in CASES {
        let (ee1, ee2, arrr) = dmp.diff_lines_to_chars(t1, t2);
        assert_eq!(ee1, Chars::from(*e1));
        assert_eq!(ee2, Chars::from(*e2));
        assert_eq!(arrr, arr.iter().map(|s| s.to_string()).collect::<Vec<_>>());
    }

    // TODO: More than 256 to reveal any 8-bit limitations
}

#[test]
fn test_diff_chars_to_lines() {
    let dmp = DiffMatchPatch::new();

    // Convert chars up to lines.
    let mut diffs = vec![Diff::Equal("\x01\x02\x01".into()), Diff::Insert("\x02\x01\x02".into())];

    dmp.diff_chars_to_lines(&mut diffs, &vec!["".into(), "alpha\n".into(), "beta\n".into()]);

    println!("=> {:?}", diffs);
    assert_eq!(
        diffs,
        vec![
            Diff::Equal("alpha\nbeta\nalpha\n".into()),
            Diff::Insert("beta\nalpha\nbeta\n".into()),
        ]
    );

    // TODO: More than 256 to reveal any 8-bit limitations.
    // TODO: More than 1,114,112 to verify any 17 * 16-bit limitation.
}
