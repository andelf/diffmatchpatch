use std::time::Duration;

#[cfg(test)]
use diffmatchpatch::*;

use Diff::*;

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
    //println!("=> {:?}", diffs);
    assert_eq!(
        diffs,
        vec![
            Diff::Equal("alpha\nbeta\nalpha\n".into()),
            Diff::Insert("beta\nalpha\nbeta\n".into()),
        ]
    );

    let n = 300;
    let mut line_list = (1..=n).map(|c| c.to_string() + "\n").collect::<Vec<_>>();
    let lines = line_list.clone().concat();
    let chars = (1..=n).map(|c| char::from_u32(c as u32).unwrap()).collect::<String>();

    line_list.insert(0, "".into());
    let mut diffs = vec![Diff::Delete(chars.into())];

    dmp.diff_chars_to_lines(&mut diffs, &line_list);
    // println!("=> {:?}", diffs);
    assert_eq!(diffs[0].text(), lines.to_chars());

    // TODO: More than 1,114,112 to verify any 17 * 16-bit limitation.
}

#[test]
fn test_diff_cleanup_merge() {
    use Diff::*;
    // Cleanup a messy diff
    let dmp = DiffMatchPatch::new();

    // Null case
    let mut diffs = vec![];
    dmp.diff_cleanup_merge(&mut diffs);
    assert!(diffs.is_empty());

    // Merge equalities
    let mut diffs = vec![Diff::Equal("a".into()), Diff::Equal("b".into()), Diff::Equal("c".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Diff::Equal("abc".into())]);

    // Merge deletions
    let mut diffs =
        vec![Diff::Delete("a".into()), Diff::Delete("b".into()), Diff::Delete("c".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Diff::Delete("abc".into())]);

    // Merge insertions
    let mut diffs =
        vec![Diff::Insert("a".into()), Diff::Insert("b".into()), Diff::Insert("c".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Diff::Insert("abc".into())]);

    // Prefix and suffix detection
    let mut diffs = vec![Delete("a".into()), Insert("abc".into()), Delete("dc".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("a".into()), Delete("d".into()), Insert("b".into()), Equal("c".into())]
    );

    // Prefix and suffix detection with equalities
    let mut diffs = vec![
        Equal("x".into()),
        Delete("a".into()),
        Insert("abc".into()),
        Delete("dc".into()),
        Equal("y".into()),
    ];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("xa".into()), Delete("d".into()), Insert("b".into()), Equal("cy".into()),]
    );

    // Slide edit left
    let mut diffs = vec![Equal("a".into()), Insert("ba".into()), Equal("c".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Insert("ab".into()), Equal("ac".into())]);

    // Slide edit right.
    let mut diffs = vec![Equal("c".into()), Insert("ab".into()), Equal("a".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Equal("ca".into()), Insert("ba".into())]);

    // Slide edit left recursive.
    let mut diffs = vec![
        Equal("a".into()),
        Delete("b".into()),
        Equal("c".into()),
        Delete("ac".into()),
        Equal("x".into()),
    ];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Delete("abc".into()), Equal("acx".into())]);

    // Slide edit right recursive.
    let mut diffs = vec![
        Equal("x".into()),
        Delete("ca".into()),
        Equal("c".into()),
        Delete("b".into()),
        Equal("a".into()),
    ];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Equal("xca".into()), Delete("cba".into())]);

    // Empty merge.
    let mut diffs = vec![Delete("b".into()), Insert("ab".into()), Equal("c".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Insert("a".into()), Equal("bc".into())]);

    // Empty equality.
    let mut diffs = vec![Equal("".into()), Insert("a".into()), Equal("b".into())];
    dmp.diff_cleanup_merge(&mut diffs);
    assert_eq!(diffs, vec![Insert("a".into()), Equal("b".into())]);
}

#[test]
fn test_diff_cleanup_semantic_lossless() {
    use Diff::*;

    // Slide diffs to match logical boundaries.
    let dmp = DiffMatchPatch::new();

    // Null case.
    let mut diffs = vec![];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert!(diffs.is_empty());

    // Blank lines.
    //diffs = [(self.dmp.DIFF_EQUAL, "AAA\r\n\r\nBBB"), (self.dmp.DIFF_INSERT, "\r\nDDD\r\n\r\nBBB"), (self.dmp.DIFF_EQUAL, "\r\nEEE")]
    //self.dmp.diff_cleanupSemanticLossless(diffs)
    //self.assertEqual([(self.dmp.DIFF_EQUAL, "AAA\r\n\r\n"), (self.dmp.DIFF_INSERT, "BBB\r\nDDD\r\n\r\n"), (self.dmp.DIFF_EQUAL, "BBB\r\nEEE")], diffs)
    let mut diffs = vec![
        Equal("AAA\r\n\r\nBBB".into()),
        Insert("\r\nDDD\r\n\r\nBBB".into()),
        Equal("\r\nEEE".into()),
    ];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(
        diffs,
        vec![
            Equal("AAA\r\n\r\n".into()),
            Insert("BBB\r\nDDD\r\n\r\n".into()),
            Equal("BBB\r\nEEE".into()),
        ]
    );

    // Line boundaries.
    //diffs = [(self.dmp.DIFF_EQUAL, "AAA\r\nBBB"), (self.dmp.DIFF_INSERT, " DDD\r\nBBB"), (self.dmp.DIFF_EQUAL, " EEE")]
    //self.dmp.diff_cleanupSemanticLossless(diffs)
    //self.assertEqual([(self.dmp.DIFF_EQUAL, "AAA\r\n"), (self.dmp.DIFF_INSERT, "BBB DDD\r\n"), (self.dmp.DIFF_EQUAL, "BBB EEE")], diffs)

    let mut diffs =
        vec![Equal("AAA\r\nBBB".into()), Insert(" DDD\r\nBBB".into()), Equal(" EEE".into())];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("AAA\r\n".into()), Insert("BBB DDD\r\n".into()), Equal("BBB EEE".into()),]
    );

    // Word boundaries.
    //     diffs = [(self.dmp.DIFF_EQUAL, "The c"), (self.dmp.DIFF_INSERT, "ow and the c"), (self.dmp.DIFF_EQUAL, "at.")]
    //     self.dmp.diff_cleanupSemanticLossless(diffs)
    //     self.assertEqual([(self.dmp.DIFF_EQUAL, "The "), (self.dmp.DIFF_INSERT, "cow and the "), (self.dmp.DIFF_EQUAL, "cat.")], diffs)

    let mut diffs = vec![Equal("The c".into()), Insert("ow and the c".into()), Equal("at.".into())];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("The ".into()), Insert("cow and the ".into()), Equal("cat.".into()),]
    );

    // Alphanumeric boundaries.
    //     diffs = [(self.dmp.DIFF_EQUAL, "The-c"), (self.dmp.DIFF_INSERT, "ow-and-the-c"), (self.dmp.DIFF_EQUAL, "at.")]
    //     self.dmp.diff_cleanupSemanticLossless(diffs)
    //     self.assertEqual([(self.dmp.DIFF_EQUAL, "The-"), (self.dmp.DIFF_INSERT, "cow-and-the-"), (self.dmp.DIFF_EQUAL, "cat.")], diffs)

    let mut diffs = vec![Equal("The-c".into()), Insert("ow-and-the-c".into()), Equal("at.".into())];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("The-".into()), Insert("cow-and-the-".into()), Equal("cat.".into()),]
    );

    // Hitting the start.
    //     diffs = [(self.dmp.DIFF_EQUAL, "a"), (self.dmp.DIFF_DELETE, "a"), (self.dmp.DIFF_EQUAL, "ax")]
    //     self.dmp.diff_cleanupSemanticLossless(diffs)
    //     self.assertEqual([(self.dmp.DIFF_DELETE, "a"), (self.dmp.DIFF_EQUAL, "aax")], diffs)

    let mut diffs = vec![Equal("a".into()), Delete("a".into()), Equal("ax".into())];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(diffs, vec![Delete("a".into()), Equal("aax".into())]);

    // Hitting the end.
    //     diffs = [(self.dmp.DIFF_EQUAL, "xa"), (self.dmp.DIFF_DELETE, "a"), (self.dmp.DIFF_EQUAL, "a")]
    //     self.dmp.diff_cleanupSemanticLossless(diffs)
    //     self.assertEqual([(self.dmp.DIFF_EQUAL, "xaa"), (self.dmp.DIFF_DELETE, "a")], diffs)

    let mut diffs = vec![Equal("xa".into()), Delete("a".into()), Equal("a".into())];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(diffs, vec![Equal("xaa".into()), Delete("a".into())]);

    // Sentence boundaries.
    //     diffs = [(self.dmp.DIFF_EQUAL, "The xxx. The "), (self.dmp.DIFF_INSERT, "zzz. The "), (self.dmp.DIFF_EQUAL, "yyy.")]
    //     self.dmp.diff_cleanupSemanticLossless(diffs)
    //     self.assertEqual([(self.dmp.DIFF_EQUAL, "The xxx."), (self.dmp.DIFF_INSERT, " The zzz."), (self.dmp.DIFF_EQUAL, " The yyy.")], diffs)

    let mut diffs =
        vec![Equal("The xxx. The ".into()), Insert("zzz. The ".into()), Equal("yyy.".into())];
    dmp.diff_cleanup_semantic_lossless(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("The xxx.".into()), Insert(" The zzz.".into()), Equal(" The yyy.".into()),]
    );
}

#[test]
fn test_diff_cleanup_semantic() {
    // Cleanup semantically trivial equalities.
    use Diff::*;

    let dmp = DiffMatchPatch::new();

    /*
    # Null case.
    diffs = []
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([], diffs)
    */

    let mut diffs = vec![];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![]);

    /*

           # No elimination #1.
           diffs = [(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "cd"), (self.dmp.DIFF_EQUAL, "12"), (self.dmp.DIFF_DELETE, "e")]
           self.dmp.diff_cleanupSemantic(diffs)
           self.assertEqual([(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "cd"), (self.dmp.DIFF_EQUAL, "12"), (self.dmp.DIFF_DELETE, "e")], diffs)
    */

    let mut diffs =
        vec![Delete("ab".into()), Insert("cd".into()), Equal("12".into()), Delete("e".into())];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(
        diffs,
        vec![Delete("ab".into()), Insert("cd".into()), Equal("12".into()), Delete("e".into()),]
    );

    /*
           # No elimination #2.
           diffs = [(self.dmp.DIFF_DELETE, "abc"), (self.dmp.DIFF_INSERT, "ABC"), (self.dmp.DIFF_EQUAL, "1234"), (self.dmp.DIFF_DELETE, "wxyz")]
           self.dmp.diff_cleanupSemantic(diffs)
           self.assertEqual([(self.dmp.DIFF_DELETE, "abc"), (self.dmp.DIFF_INSERT, "ABC"), (self.dmp.DIFF_EQUAL, "1234"), (self.dmp.DIFF_DELETE, "wxyz")], diffs)

    */

    let mut diffs = vec![
        Delete("abc".into()),
        Insert("ABC".into()),
        Equal("1234".into()),
        Delete("wxyz".into()),
    ];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(
        diffs,
        vec![
            Delete("abc".into()),
            Insert("ABC".into()),
            Equal("1234".into()),
            Delete("wxyz".into()),
        ]
    );

    /*
    # Simple elimination.
    diffs = [(self.dmp.DIFF_DELETE, "a"), (self.dmp.DIFF_EQUAL, "b"), (self.dmp.DIFF_DELETE, "c")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abc"), (self.dmp.DIFF_INSERT, "b")], diffs)
    */

    let mut diffs = vec![Delete("a".into()), Equal("b".into()), Delete("c".into())];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![Delete("abc".into()), Insert("b".into())]);

    /*
    # Backpass elimination.
    diffs = [(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_EQUAL, "cd"), (self.dmp.DIFF_DELETE, "e"), (self.dmp.DIFF_EQUAL, "f"), (self.dmp.DIFF_INSERT, "g")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abcdef"), (self.dmp.DIFF_INSERT, "cdfg")], diffs)
    */

    let mut diffs = vec![
        Delete("ab".into()),
        Equal("cd".into()),
        Delete("e".into()),
        Equal("f".into()),
        Insert("g".into()),
    ];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![Delete("abcdef".into()), Insert("cdfg".into())]);

    /*       # Multiple eliminations.
    diffs = [(self.dmp.DIFF_INSERT, "1"), (self.dmp.DIFF_EQUAL, "A"), (self.dmp.DIFF_DELETE, "B"), (self.dmp.DIFF_INSERT, "2"), (self.dmp.DIFF_EQUAL, "_"), (self.dmp.DIFF_INSERT, "1"), (self.dmp.DIFF_EQUAL, "A"), (self.dmp.DIFF_DELETE, "B"), (self.dmp.DIFF_INSERT, "2")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "AB_AB"), (self.dmp.DIFF_INSERT, "1A2_1A2")], diffs)
    */

    let mut diffs = vec![
        Insert("1".into()),
        Equal("A".into()),
        Delete("B".into()),
        Insert("2".into()),
        Equal("_".into()),
        Insert("1".into()),
        Equal("A".into()),
        Delete("B".into()),
        Insert("2".into()),
    ];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![Delete("AB_AB".into()), Insert("1A2_1A2".into())]);

    /*

    # Word boundaries.
    diffs = [(self.dmp.DIFF_EQUAL, "The c"), (self.dmp.DIFF_DELETE, "ow and the c"), (self.dmp.DIFF_EQUAL, "at.")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_EQUAL, "The "), (self.dmp.DIFF_DELETE, "cow and the "), (self.dmp.DIFF_EQUAL, "cat.")], diffs)
    */

    let mut diffs = vec![Equal("The c".into()), Delete("ow and the c".into()), Equal("at.".into())];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(
        diffs,
        vec![Equal("The ".into()), Delete("cow and the ".into()), Equal("cat.".into()),]
    );

    /*

    # No overlap elimination.
    diffs = [(self.dmp.DIFF_DELETE, "abcxx"), (self.dmp.DIFF_INSERT, "xxdef")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abcxx"), (self.dmp.DIFF_INSERT, "xxdef")], diffs)
    */

    let mut diffs = vec![Delete("abcxx".into()), Insert("xxdef".into())];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![Delete("abcxx".into()), Insert("xxdef".into())]);

    /*

    # Overlap elimination.
    diffs = [(self.dmp.DIFF_DELETE, "abcxxx"), (self.dmp.DIFF_INSERT, "xxxdef")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abc"), (self.dmp.DIFF_EQUAL, "xxx"), (self.dmp.DIFF_INSERT, "def")], diffs)
    */

    let mut diffs = vec![Delete("abcxxx".into()), Insert("xxxdef".into())];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![Delete("abc".into()), Equal("xxx".into()), Insert("def".into())]);

    /*
    # Reverse overlap elimination.
    diffs = [(self.dmp.DIFF_DELETE, "xxxabc"), (self.dmp.DIFF_INSERT, "defxxx")]
    self.dmp.diff_cleanupSemantic(diffs)
    self.assertEqual([(self.dmp.DIFF_INSERT, "def"), (self.dmp.DIFF_EQUAL, "xxx"), (self.dmp.DIFF_DELETE, "abc")], diffs)
    */

    let mut diffs = vec![Delete("xxxabc".into()), Insert("defxxx".into())];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(diffs, vec![Insert("def".into()), Equal("xxx".into()), Delete("abc".into())]);

    /*

       # Two overlap eliminations.
       diffs = [(self.dmp.DIFF_DELETE, "abcd1212"), (self.dmp.DIFF_INSERT, "1212efghi"), (self.dmp.DIFF_EQUAL, "----"), (self.dmp.DIFF_DELETE, "A3"), (self.dmp.DIFF_INSERT, "3BC")]
       self.dmp.diff_cleanupSemantic(diffs)
       self.assertEqual([(self.dmp.DIFF_DELETE, "abcd"), (self.dmp.DIFF_EQUAL, "1212"), (self.dmp.DIFF_INSERT, "efghi"), (self.dmp.DIFF_EQUAL, "----"), (self.dmp.DIFF_DELETE, "A"), (self.dmp.DIFF_EQUAL, "3"), (self.dmp.DIFF_INSERT, "BC")], diffs)

    */

    let mut diffs = vec![
        Delete("abcd1212".into()),
        Insert("1212efghi".into()),
        Equal("----".into()),
        Delete("A3".into()),
        Insert("3BC".into()),
    ];
    dmp.diff_cleanup_semantic(&mut diffs);
    assert_eq!(
        diffs,
        vec![
            Delete("abcd".into()),
            Equal("1212".into()),
            Insert("efghi".into()),
            Equal("----".into()),
            Delete("A".into()),
            Equal("3".into()),
            Insert("BC".into()),
        ]
    );
}

#[test]
fn test_diff_cleanup_efficiency() {
    // Cleanup operationally trivial equalities.
    let mut dmp = DiffMatchPatch::new();
    dmp.edit_cost = 4;

    /*
    # Null case.
    diffs = []
    self.dmp.diff_cleanupEfficiency(diffs)
    self.assertEqual([], diffs)
    */

    let mut diffs = vec![];
    dmp.diff_cleanup_efficiency(&mut diffs);
    assert_eq!(diffs, vec![]);

    /*
    # No elimination.
    diffs = [(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "12"), (self.dmp.DIFF_EQUAL, "wxyz"), (self.dmp.DIFF_DELETE, "cd"), (self.dmp.DIFF_INSERT, "34")]
    self.dmp.diff_cleanupEfficiency(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "12"), (self.dmp.DIFF_EQUAL, "wxyz"), (self.dmp.DIFF_DELETE, "cd"), (self.dmp.DIFF_INSERT, "34")], diffs)
    */

    let mut diffs = vec![
        Delete("ab".into()),
        Insert("12".into()),
        Equal("wxyz".into()),
        Delete("cd".into()),
        Insert("34".into()),
    ];
    dmp.diff_cleanup_efficiency(&mut diffs);
    assert_eq!(
        diffs,
        vec![
            Delete("ab".into()),
            Insert("12".into()),
            Equal("wxyz".into()),
            Delete("cd".into()),
            Insert("34".into()),
        ]
    );

    /*
    # Four-edit elimination.
    diffs = [(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "12"), (self.dmp.DIFF_EQUAL, "xyz"), (self.dmp.DIFF_DELETE, "cd"), (self.dmp.DIFF_INSERT, "34")]
    self.dmp.diff_cleanupEfficiency(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abxyzcd"), (self.dmp.DIFF_INSERT, "12xyz34")], diffs)
    */

    let mut diffs = vec![
        Delete("ab".into()),
        Insert("12".into()),
        Equal("xyz".into()),
        Delete("cd".into()),
        Insert("34".into()),
    ];
    dmp.diff_cleanup_efficiency(&mut diffs);
    assert_eq!(diffs, vec![Delete("abxyzcd".into()), Insert("12xyz34".into())]);

    /*
    # Three-edit elimination.
    diffs = [(self.dmp.DIFF_INSERT, "12"), (self.dmp.DIFF_EQUAL, "x"), (self.dmp.DIFF_DELETE, "cd"), (self.dmp.DIFF_INSERT, "34")]
    self.dmp.diff_cleanupEfficiency(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "xcd"), (self.dmp.DIFF_INSERT, "12x34")], diffs)
    */

    let mut diffs =
        vec![Insert("12".into()), Equal("x".into()), Delete("cd".into()), Insert("34".into())];
    dmp.diff_cleanup_efficiency(&mut diffs);
    assert_eq!(diffs, vec![Delete("xcd".into()), Insert("12x34".into())]);

    /*
    # Backpass elimination.
    diffs = [(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "12"), (self.dmp.DIFF_EQUAL, "xy"), (self.dmp.DIFF_INSERT, "34"), (self.dmp.DIFF_EQUAL, "z"), (self.dmp.DIFF_DELETE, "cd"), (self.dmp.DIFF_INSERT, "56")]
    self.dmp.diff_cleanupEfficiency(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abxyzcd"), (self.dmp.DIFF_INSERT, "12xy34z56")], diffs)
    */

    let mut diffs = vec![
        Delete("ab".into()),
        Insert("12".into()),
        Equal("xy".into()),
        Insert("34".into()),
        Equal("z".into()),
        Delete("cd".into()),
        Insert("56".into()),
    ];
    dmp.diff_cleanup_efficiency(&mut diffs);
    assert_eq!(diffs, vec![Delete("abxyzcd".into()), Insert("12xy34z56".into())]);

    /*
    # High cost elimination.
    self.dmp.Diff_EditCost = 5
    diffs = [(self.dmp.DIFF_DELETE, "ab"), (self.dmp.DIFF_INSERT, "12"), (self.dmp.DIFF_EQUAL, "wxyz"), (self.dmp.DIFF_DELETE, "cd"), (self.dmp.DIFF_INSERT, "34")]
    self.dmp.diff_cleanupEfficiency(diffs)
    self.assertEqual([(self.dmp.DIFF_DELETE, "abwxyzcd"), (self.dmp.DIFF_INSERT, "12wxyz34")], diffs)
    self.dmp.Diff_EditCost = 4
     */

    dmp.edit_cost = 5;
    let mut diffs = vec![
        Delete("ab".into()),
        Insert("12".into()),
        Equal("wxyz".into()),
        Delete("cd".into()),
        Insert("34".into()),
    ];
    dmp.diff_cleanup_efficiency(&mut diffs);
    assert_eq!(diffs, vec![Delete("abwxyzcd".into()), Insert("12wxyz34".into())]);
}

#[test]
fn test_diff_text() {
    // Compute the source and destination texts.
    let dmp = DiffMatchPatch::new();

    let diffs = vec![
        Equal("jump".into()),
        Delete("s".into()),
        Insert("ed".into()),
        Equal(" over ".into()),
        Delete("the".into()),
        Insert("a".into()),
        Equal(" lazy".into()),
    ];
    assert_eq!(Chars::from("jumps over the lazy"), dmp.diff_text1(&diffs));
    assert_eq!(Chars::from("jumped over a lazy"), dmp.diff_text2(&diffs));
}

#[test]
fn test_diff_xindex() {
    // Translate a location in text1 to text2.

    let dmp = DiffMatchPatch::new();

    //self.assertEqual(5, self.dmp.diff_xIndex([(self.dmp.DIFF_DELETE, "a"), (self.dmp.DIFF_INSERT, "1234"), (self.dmp.DIFF_EQUAL, "xyz")], 2))
    let diffs = vec![Delete("a".into()), Insert("1234".into()), Equal("xyz".into())];
    assert_eq!(5, dmp.diff_xindex(&diffs, 2));

    // Translation on deletion.
    //self.assertEqual(1, self.dmp.diff_xIndex([(self.dmp.DIFF_EQUAL, "a"), (self.dmp.DIFF_DELETE, "1234"), (self.dmp.DIFF_EQUAL, "xyz")], 3))
    let diffs = vec![Equal("a".into()), Delete("1234".into()), Equal("xyz".into())];
    assert_eq!(1, dmp.diff_xindex(&diffs, 3));
}
