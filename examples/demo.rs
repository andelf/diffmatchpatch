//! The same example as <https://neil.fraser.name/software/diff_match_patch/demos/diff.html>.
use diffmatchpatch::prelude::*;

fn main() {
    let text1 = r#"I am the very model of a modern Major-General,
I've information vegetable, animal, and mineral,
I know the kings of England, and I quote the fights historical,
From Marathon to Waterloo, in order categorical."#;
    let text2 = r#"I am the very model of a cartoon individual,
My animation's comical, unusual, and whimsical,
I'm quite adept at funny gags, comedic theory I have read,
From wicked puns and stupid jokes to anvils that drop on your head."#;

    // No Cleanup
    //let diffs = diff_main(text2, text1);
    //println!("diffs {:#?}", diffs);

    // Semantic Cleanup
    let diffs = diff_semantic(text2, text1);
    println!("Semantic diffs {:#?}", diffs);

    // Word mode
    let diffs = diff_word_mode(text2, text1);
    println!("Word diffs {:#?}", diffs);
}
