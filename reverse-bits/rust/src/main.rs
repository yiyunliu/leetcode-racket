struct Solution;
impl Solution {
    pub fn reverse_bits(mut n: i32) -> i32 {
	let mut acc = 0;
	for i in 0..32 {
	    acc <<= 1;
	    let bit = n & 1;
	    n >>= 1;
	    acc ^= bit;
	}
	acc
    }
}


fn main() {
    println!("Hello, world!");
}
