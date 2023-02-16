use diffmatchpatch::*;

pub trait BlockExt {
    fn is_block_start(&self) -> bool;
    fn block_level_prefix(&self) -> &str;
    fn is_whitespace_start(&self) -> bool;
}

impl BlockExt for str {
    fn is_block_start(&self) -> bool {
        self.starts_with("- ") || self.starts_with("-\n")
    }

    fn block_level_prefix(&self) -> &str {
        &self[..self.find('-').unwrap()]
    }

    fn is_whitespace_start(&self) -> bool {
        let c = self.as_bytes()[0];
        c == b'\t' || c == b' ' || c == b'\x09' || c == b'\x0c'
    }
}

pub fn split_blocks(page: &str) -> Vec<&str> {
    let mut blocks = vec![];

    let mut start = 0;
    let mut end = 0;

    let mut it = page.split_inclusive('\n');
    let mut prefix = "";
    loop {
        match it.next() {
            None => {
                if start != end {
                    blocks.push(&page[start..end]);
                }
                break;
            }
            Some(line) => {
                // cont.
                if line.trim().is_empty() {
                    end += line.len();
                } else if line.is_whitespace_start() {
                    if line.starts_with(prefix) {
                        let bare_line = line.trim_start_matches(prefix);
                        if bare_line.is_block_start() {
                            // new block one the same level
                            if start != end {
                                blocks.push(&page[start..end]);
                            }
                            start = end;
                            end = start + line.len();
                        } else if bare_line.trim_start().is_block_start() {
                            // new block on sub level
                            if start != end {
                                blocks.push(&page[start..end]);
                            }
                            start = end;
                            end = start + line.len();
                            prefix = line.block_level_prefix()
                        } else {
                            end += line.len();
                        }
                    } else if line.trim_start().is_block_start() {
                        // new block on parent level
                        if start != end {
                            blocks.push(&page[start..end]);
                        }
                        start = end;
                        end = start + line.len();
                        prefix = line.block_level_prefix();
                    } else {
                        // block cont.
                        end += line.len();
                    }
                } else if line.is_block_start() {
                    // new block
                    if start != end {
                        blocks.push(&page[start..end]);
                    }
                    start = end;
                    end = start + line.len();
                    prefix = "";
                } else if prefix == "" && line.contains(":: ") {
                    // global property line
                    if start != end {
                        blocks.push(&page[start..end]);
                    }
                    start = end;
                    end = start + line.len();
                    prefix = "";
                } else {
                    end += line.len();
                }
            }
        }
    }
    blocks
}

fn main() {
    let a = r"- Block 1
    - Block 2
    - Block 3";
    let b = r"- Block 1
    - Block 4
    - Block 3";
    let ab = split_blocks(a);
    let bb = split_blocks(b);

    let mut dmp = DiffMatchPatch::new();

    let (ac, bc, item_array) = dmp.diff_any_to_chars(&ab, &bb);

    println!("{:?} {:?}\n{:#?}", ac, bc, item_array);
    let mut diffs = dmp.diff_main(&ac, &bc, false);

    //dmp.diff_cleanup_merge(&mut diffs);
    dmp.diff_cleanup_efficiency(&mut diffs);

    let new_diffs = dmp.diff_chars_to_any(&mut diffs, &item_array);

    println!("=> {:#?}", new_diffs);
    println!("diffs => {}", new_diffs.len());
}
