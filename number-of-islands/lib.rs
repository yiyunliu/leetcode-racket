struct Solution;

impl Solution {
    pub fn hamming_weight(n : i32) -> i32 {
	let mut acc = 0;
	for i in 0..32 {
	    acc += ((1 << i) & n) >> i;
	}
	acc
    }
}
