//! PATCH TEST FUNCTIONS

#[cfg(test)]
use diffmatchpatch::*;

#[test]
fn patch_obj() {
    /*
         p = dmp_module.patch_obj()
       p.start1 = 20
       p.start2 = 21
       p.length1 = 18
       p.length2 = 17
       p.diffs = [(self.dmp.DIFF_EQUAL, "jump"), (self.dmp.DIFF_DELETE, "s"), (self.dmp.DIFF_INSERT, "ed"), (self.dmp.DIFF_EQUAL, " over "), (self.dmp.DIFF_DELETE, "the"), (self.dmp.DIFF_INSERT, "a"), (self.dmp.DIFF_EQUAL, "\nlaz")]
       strp = str(p)
       self.assertEqual("@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n", strp)

    */
    let p = PatchObj {
        start1: Some(20),
        start2: Some(21),
        length1: 18,
        length2: 17,
        diffs: vec![
            Diff::Equal("jump".into()),
            Diff::Delete("s".into()),
            Diff::Insert("ed".into()),
            Diff::Equal(" over ".into()),
            Diff::Delete("the".into()),
            Diff::Insert("a".into()),
            Diff::Equal("\nlaz".into()),
        ],
    };
    let strp = format!("{p}");
    println!("{strp}");
    assert_eq!("@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n", strp);
}
