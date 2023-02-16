# DiffMatchPatch - diffmatchpatch

The Diff Match and Patch libraries offer robust algorithms to perform the operations required for synchronising plain text. This repository contains a Rust version of the original [diff-match-patch](https://github.com/google/diff-match-patch) library, using up-to-date crate packages.

## Modification Compared to the Upstream

- Use `Chars(Vec<char>)` to represent text instead of `String` to avoid unnecessary traversing of the string
- Extend `diff_lines_to_chars` to `diff_any_to_chars` to support lines, text blocks, any sequence of comparable items
- Test cases from [distill-io/diff-match-patch.rs](https://github.com/distill-io/diff-match-patch.rs)

## Features

- Diffing and patching library for plain text
- Retrieve differences between two blocks of text
- Create a set of patches for converting a block of text into another
- Apply a set of patches onto a block of text to convert it to another block of text
- Uses best-effort to apply patch even when the underlying text doesn't fully match.

## Original

This code is forked originally from [dmp](https://github.com/surrealdb/dmp), licensed under the [MIT](https://choosealicense.com/licenses/mit/) license.

## speedtest

```text
Python3 Elapsed Time: 8.695004s
JS(Chrome) Time: 0.469s
speedtest               time:   147.20 ms
```
