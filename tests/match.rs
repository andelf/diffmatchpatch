use std::collections::HashMap;

#[cfg(test)]
use diffmatchpatch::*;

#[test]
fn match_alphabet() {
    // Initialise the bitmasks for Bitap.
    // self.assertEqual({"a":4, "b":2, "c":1}, self.dmp.match_alphabet("abc"))
    let mut dmp = DiffMatchPatch::new();

    let mut s = HashMap::new();
    s.insert('a', 4);
    s.insert('b', 2);
    s.insert('c', 1);
    assert_eq!(s, dmp.match_alphabet(&"abc".to_chars()));

    // self.assertEqual({"a":37, "b":18, "c":8}, self.dmp.match_alphabet("abcaba"))
    let mut s = HashMap::new();
    s.insert('a', 37);
    s.insert('b', 18);
    s.insert('c', 8);
    assert_eq!(s, dmp.match_alphabet(&"abcaba".to_chars()));
}

#[test]
fn match_bitap() {
    let mut dmp = DiffMatchPatch::new();
    dmp.match_distance = 100;
    dmp.match_threshold = 0.5;

    // Exact matches.
    assert_eq!(Some(5), dmp.match_bitap(&"abcdefghijk".to_chars(), &"fgh".to_chars(), 5));
    assert_eq!(Some(5), dmp.match_bitap(&"abcdefghijk".to_chars(), &"fgh".to_chars(), 0));

    // Fuzzy matches.
    //self.assertEqual(4, self.dmp.match_bitap("abcdefghijk", "efxhi", 0))
    assert_eq!(Some(4), dmp.match_bitap(&"abcdefghijk".to_chars(), &"efxhi".to_chars(), 0));

    //self.assertEqual(2, self.dmp.match_bitap("abcdefghijk", "cdefxyhijk", 5))
    assert_eq!(Some(2), dmp.match_bitap(&"abcdefghijk".to_chars(), &"cdefxyhijk".to_chars(), 5));

    //self.assertEqual(-1, self.dmp.match_bitap("abcdefghijk", "bxy", 1))
    assert_eq!(None, dmp.match_bitap(&"abcdefghijk".to_chars(), &"bxy".to_chars(), 1));

    // Overflow.
    //self.assertEqual(2, self.dmp.match_bitap("123456789xx0", "3456789x0", 2))
    assert_eq!(Some(2), dmp.match_bitap(&"123456789xx0".to_chars(), &"3456789x0".to_chars(), 2));

    //self.assertEqual(0, self.dmp.match_bitap("abcdef", "xxabc", 4))
    assert_eq!(Some(0), dmp.match_bitap(&"abcdef".to_chars(), &"xxabc".to_chars(), 4));

    //self.assertEqual(3, self.dmp.match_bitap("abcdef", "defyy", 4))
    assert_eq!(Some(3), dmp.match_bitap(&"abcdef".to_chars(), &"defyy".to_chars(), 4));

    //self.assertEqual(0, self.dmp.match_bitap("abcdef", "xabcdefy", 0))
    assert_eq!(Some(0), dmp.match_bitap(&"abcdef".to_chars(), &"xabcdefy".to_chars(), 0));

    // Threshold test.
    //dmp.Match_Threshold = 0.4
    //self.assertEqual(4, self.dmp.match_bitap("abcdefghijk", "efxyhi", 1))
    dmp.match_threshold = 0.4;
    assert_eq!(Some(4), dmp.match_bitap(&"abcdefghijk".to_chars(), &"efxyhi".to_chars(), 1));

    //self.dmp.Match_Threshold = 0.3
    //self.assertEqual(-1, self.dmp.match_bitap("abcdefghijk", "efxyhi", 1))
    dmp.match_threshold = 0.3;
    assert_eq!(None, dmp.match_bitap(&"abcdefghijk".to_chars(), &"efxyhi".to_chars(), 1));

    //self.dmp.Match_Threshold = 0.0
    // self.assertEqual(1, self.dmp.match_bitap("abcdefghijk", "bcdef", 1))
    dmp.match_threshold = 0.0;
    assert_eq!(Some(1), dmp.match_bitap(&"abcdefghijk".to_chars(), &"bcdef".to_chars(), 1));
    dmp.match_threshold = 0.5;

    // Multiple select.
    //self.assertEqual(0, self.dmp.match_bitap("abcdexyzabcde", "abccde", 3))
    assert_eq!(Some(0), dmp.match_bitap(&"abcdexyzabcde".to_chars(), &"abccde".to_chars(), 3));

    // self.assertEqual(8, self.dmp.match_bitap("abcdexyzabcde", "abccde", 5))
    assert_eq!(Some(8), dmp.match_bitap(&"abcdexyzabcde".to_chars(), &"abccde".to_chars(), 5));

    // Distance test.
    //self.dmp.Match_Distance = 10  # Strict location.
    //self.assertEqual(-1, self.dmp.match_bitap("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24))
    dmp.match_distance = 10;
    assert_eq!(
        None,
        dmp.match_bitap(&"abcdefghijklmnopqrstuvwxyz".to_chars(), &"abcdefg".to_chars(), 24)
    );

    //self.assertEqual(0, self.dmp.match_bitap("abcdefghijklmnopqrstuvwxyz", "abcdxxefg", 1))
    assert_eq!(
        Some(0),
        dmp.match_bitap(&"abcdefghijklmnopqrstuvwxyz".to_chars(), &"abcdxxefg".to_chars(), 1)
    );

    //self.dmp.Match_Distance = 1000  # Loose location.
    //self.assertEqual(0, self.dmp.match_bitap("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24))

    dmp.match_distance = 1000;
    assert_eq!(
        Some(0),
        dmp.match_bitap(&"abcdefghijklmnopqrstuvwxyz".to_chars(), &"abcdefg".to_chars(), 24)
    );
}

#[test]
fn match_main() {
    let mut dmp = DiffMatchPatch::new();
    // Shortcut matches.
    //self.assertEqual(0, self.dmp.match_main("abcdef", "abcdef", 1000))
    assert_eq!(Some(0), dmp.match_main(&"abcdef".to_chars(), &"abcdef".to_chars(), 1000));

    //self.assertEqual(-1, self.dmp.match_main("", "abcdef", 1))
    assert_eq!(None, dmp.match_main(&"".to_chars(), &"abcdef".to_chars(), 1));

    //self.assertEqual(3, self.dmp.match_main("abcdef", "", 3))
    assert_eq!(Some(3), dmp.match_main(&"abcdef".to_chars(), &"".to_chars(), 3));

    //self.assertEqual(3, self.dmp.match_main("abcdef", "de", 3))
    assert_eq!(Some(3), dmp.match_main(&"abcdef".to_chars(), &"de".to_chars(), 3));

    //self.assertEqual(3, self.dmp.match_main("abcdef", "defy", 4))
    assert_eq!(Some(3), dmp.match_main(&"abcdef".to_chars(), &"defy".to_chars(), 4));

    //self.assertEqual(0, self.dmp.match_main("abcdef", "abcdefy", 0))
    assert_eq!(Some(0), dmp.match_main(&"abcdef".to_chars(), &"abcdefy".to_chars(), 0));

    // Complex match.
    dmp.match_threshold = 0.7;
    //  self.assertEqual(4, self.dmp.match_main("I am the very model of a modern major general.", " that berry ", 5))
    assert_eq!(
        Some(4),
        dmp.match_main(
            &"I am the very model of a modern major general.".to_chars(),
            &" that berry ".to_chars(),
            5
        )
    );
}
