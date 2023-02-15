//! MATCH FUNCTIONS

use std::collections::HashMap;

use crate::DiffMatchPatch;

impl DiffMatchPatch {
    /**
      Locate the best instance of 'pattern' in 'text' near 'loc'.

      Args:
          text: The text to search.
          pattern: The pattern to search for.
          loc: The location to search around.

      Returns:
          Best match index or -1.
    */
    pub fn match_main(&mut self, text: &[char], pattern: &[char], mut loc: usize) -> Option<usize> {
        loc = usize::min(loc, text.len());

        if text == pattern {
            // Shortcut (potentially not guaranteed by the algorithm)
            return Some(0);
        } else if text.is_empty() {
            // Nothing to match.
            return None;
        } else if loc + pattern.len() <= text.len()
            && text[(loc)..(loc + pattern.len())].to_vec() == pattern
        {
            // Perfect match at the perfect spot!  (Includes case of null pattern)
            return Some(loc);
        }
        self.match_bitap(text, pattern, loc)
    }

    /**
      Locate the best instance of 'pattern' in 'text' near 'loc' using the
      Bitap algorithm.

      Args:
          text: The text to search.
          pattern: The pattern to search for.
          loc: The location to search around.

      Returns:
          Best match index or -1.
    */
    pub fn match_bitap(&mut self, text: &[char], patern: &[char], loc: usize) -> Option<usize> {
        // check for maxbits limit.
        if !(self.match_maxbits == 0 || patern.len() as i32 <= self.match_maxbits) {
            panic!("patern too long for this application");
        }
        // Initialise the alphabet.
        let s: HashMap<char, i32> = self.match_alphabet(patern);

        // Highest score beyond which we give up.
        let mut score_threshold: f32 = self.match_threshold;
        // Is there a nearby exact match? (speedup)
        let best_loc = self.kmp(text, patern, loc);
        if let Some(best_loc) = best_loc {
            score_threshold = f32::min(
                self.match_bitap_score(0, best_loc as i32, loc as i32, patern),
                score_threshold,
            );
            // What about in the other direction? (speedup)
            if let Some(best_loc) = self.rkmp(text, patern, loc + patern.len()) {
                score_threshold = f32::min(
                    score_threshold,
                    self.match_bitap_score(0, best_loc as i32, loc as i32, patern),
                );
            }
        }
        // Initialise the bit arrays.
        let matchmask = 1 << (patern.len() - 1); //>
        let mut best_loc: i32 = -1;
        let mut bin_min: i32;
        let mut bin_mid: i32;
        let mut bin_max: i32 = (patern.len() + text.len()) as i32;
        // Empty initialization added to appease pychecker.
        let mut last_rd: Vec<i32> = vec![];
        for d in 0..patern.len() {
            /*
            Scan for the best match each iteration allows for one more error.
            Run a binary search to determine how far from 'loc' we can stray at
            this error level.
            */
            let mut rd: Vec<i32> = vec![];
            bin_min = 0;
            bin_mid = bin_max;
            // Use the result from this iteration as the maximum for the next.
            while bin_min < bin_mid {
                if self.match_bitap_score(d as i32, (loc as i32) + bin_mid, loc as i32, patern)
                    <= score_threshold
                {
                    bin_min = bin_mid;
                } else {
                    bin_max = bin_mid;
                }
                bin_mid = bin_min + (bin_max - bin_min) / 2;
            }
            bin_max = bin_mid;
            let mut start = i32::max(1, loc as i32 - bin_mid + 1);
            let finish = i32::min(loc as i32 + bin_mid, text.len() as i32) + patern.len() as i32;
            rd.resize((finish + 2) as usize, 0);
            rd[(finish + 1) as usize] = (1 << d) - 1; //>
            let mut j = finish;
            while j >= start {
                let char_match: i32;
                if text.len() < j as usize {
                    // Out of range.
                    char_match = 0;
                } else {
                    // Subsequent passes: fuzzy match.
                    match s.get(&(text[j as usize - 1])) {
                        Some(num) => {
                            char_match = *num;
                        }
                        None => {
                            char_match = 0;
                        }
                    }
                }
                if d == 0 {
                    // First pass: exact match.
                    rd[j as usize] = ((rd[j as usize + 1] << 1) | 1) & char_match;
                //>>
                } else {
                    rd[j as usize] = (((rd[j as usize + 1] << 1) | 1) & char_match)
                        | (((last_rd[j as usize + 1] | last_rd[j as usize]) << 1) | 1)
                        | last_rd[j as usize + 1]; //>>>>
                }
                if (rd[j as usize] & matchmask) != 0 {
                    let score: f32 = self.match_bitap_score(d as i32, j - 1, loc as i32, patern);
                    // This match will almost certainly be better than any existing match.
                    // But check anyway.
                    if score <= score_threshold {
                        // Told you so.
                        score_threshold = score;
                        best_loc = j - 1;
                        if best_loc as usize > loc {
                            // When passing loc, don't exceed our current distance from loc.
                            start = i32::max(1, 2 * loc as i32 - best_loc);
                        } else {
                            // Already passed loc, downhill from here on in.
                            break;
                        }
                    }
                }
                j -= 1;
            }
            // No hope for a (better) match at greater error levels.
            if self.match_bitap_score(d as i32 + 1, loc as i32, loc as i32, patern)
                > score_threshold
            {
                break;
            }
            last_rd = rd;
        }
        if best_loc == -1 {
            None
        } else {
            Some(best_loc as usize)
        }
    }

    /**
      Initialise the alphabet for the Bitap algorithm.

      Args:
          pattern: The text to encode.

      Returns:
          Hash of character locations.
    */

    pub fn match_alphabet(&mut self, patern: &[char]) -> HashMap<char, i32> {
        let mut s: HashMap<char, i32> = HashMap::new();
        for patern_item in patern {
            s.insert(*patern_item, 0);
        }
        for i in 0..patern.len() {
            let ch: char = patern[i];
            let mut temp: i32 = 0;
            if let Some(num) = s.get(&ch) {
                temp = num | (1 << (patern.len() - i - 1)); //>>
            }
            s.insert(ch, temp);
        }
        s
    }

    /**
    Compute and return the score for a match with e errors and x location.
    Accesses loc and pattern through being a closure.

    Args:
        e: Number of errors in match.
        x: Location of match.

    Returns:
        Overall score for match (0.0 = good, 1.0 = bad).
    */
    fn match_bitap_score(&mut self, e: i32, x: i32, loc: i32, patern: &[char]) -> f32 {
        let accuracy: f32 = (e as f32) / (patern.len() as f32);
        let proximity: i32 = (loc - x).abs();
        if self.match_distance == 0 {
            // Dodge divide by zero error.
            if proximity == 0 {
                return accuracy;
            } else {
                return 1.0;
            }
        }
        accuracy + ((proximity as f32) / (self.match_distance as f32))
    }
}
