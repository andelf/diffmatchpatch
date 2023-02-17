# DiffMatchPatch - diffmatchpatch

The Diff Match and Patch libraries offer robust algorithms to perform the operations required for synchronising plain text. This repository contains a Rust version of the original [diff-match-patch](https://github.com/google/diff-match-patch) library, using up-to-date crate packages.

## Modification Compared to the Upstream

- Use `Chars(Vec<char>)` to represent text instead of `String` to avoid unnecessary traversing of the string
- Extend `diff_lines_to_chars` to `diff_any_to_chars` to support words, lines, text blocks, and any sequence of comparable items
- Test cases from [google/diff-match-patch](https://github.com/google/diff-match-patch) and [distill-io/diff-match-patch.rs](https://github.com/distill-io/diff-match-patch.rs)
- [ ] Implement the `patch` part
- Helper `prelude` mod

## Demo

The same example is copied from <https://neil.fraser.name/software/diff_match_patch/demos/diff.html>.

```rust
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

    // diff_word_mode is also avaliable, but is meaningless to this example
}
```

## Original

This code is forked originally from [dmp](https://github.com/surrealdb/dmp), licensed under the [MIT](https://choosealicense.com/licenses/mit/) license.

### Features

- Diffing and patching library for plain text
- Retrieve differences between two blocks of text
- Create a set of patches for converting a block of text into another
- Apply a set of patches onto a block of text to convert it to another block of text
- Uses best-effort to apply patch even when the underlying text doesn't fully match.

## speedtest

On an M1 Pro MacBook Pro:

```text
Python3                         8.695004s
JS(Chrome)                      0.469s
speedtest                       147.20 ms
```
