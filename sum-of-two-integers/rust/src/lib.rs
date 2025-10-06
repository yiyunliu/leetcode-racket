pub struct Solution;

impl Solution {
    pub fn get_sum(a: i32, b: i32) -> i32 {
	let mut carry = 0;
	let mut acc = 0;
	for i in 0..32 {
	    let a_bit = (a >> i) & 1 ;
	    let b_bit = (b >> i) & 1;
	    let xor_ab = a_bit ^ b_bit;
	    let xor_abc = xor_ab ^ carry;
	    let and_xor_ab_c = xor_ab & carry;
	    let and_ab = a_bit & b_bit;
	    carry = and_xor_ab_c | and_ab;
	    acc |= xor_abc << i;
	}
	acc
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
	assert_eq!(Solution::get_sum(1,1),2);
    }
}
