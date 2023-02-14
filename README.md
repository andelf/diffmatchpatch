# DiffMatchPatch

The Diff Match and Patch libraries offer robust algorithms to perform the operations required for synchronising plain text. This repository contains a Rust version of the original [diff-match-patch](https://github.com/google/diff-match-patch) library, using up-to-date crate packages.

## Features

- Diffing and patching library for plain text
- Retrieve differences between two blocks of text
- Create a set of patches for converting a block of text into another
- Apply a set of patches onto a block of text to convert it to another block of text
- Uses best-effort to apply patch even when the underlying text doesn't fully match.

## Original

This code is forked originally from [dmp](https://github.com/surrealdb/dmp), licensed under the [MIT](https://choosealicense.com/licenses/mit/) license.

## speedtest

Python3 Elapsed Time: 8.695004s
JS(Chrome) Time: 0.469s

```text
speedtest               time:   [147.20 ms 147.24 ms 147.29 ms]
```
