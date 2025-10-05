use std::collections::HashMap as Map;

pub struct Solution;

impl Solution {
    pub fn character_replacement(s: String, k: i32) -> i32 {
	let a_u8 : u8 = 65;
	let mut counts: [usize;26] = [0;26];
	let k = k as usize;
	let s = s.into_bytes();
	let mut start : usize = 0;
	let mut end : usize = 0;
	let mut max : usize = 0;

	while end < s.len() {
	    let size = end - start;
	    let next_elem = s[end] - a_u8;
	    counts[next_elem as usize] += 1;
	    let highest_freq = *counts.iter().max().unwrap();

	    if highest_freq + k >= size + 1 {
		end += 1;
	    }
	    else {
		start += 1;
		counts[(s[start] - a_u8) as usize] -= 1;
		counts[next_elem as usize] -= 1;
	    }
	    max = std::cmp::max(max, end - start);
	}
	max as i32
    }
}


#[cfg(test)]
mod tests {
    use super::Solution;

    #[test]
    fn test0() {
	let result = Solution::character_replacement("".to_string(), 2);
	assert_eq!(result, 0);
    }

    #[test]
    fn test1() {
	let result = Solution::character_replacement("ABAB".to_string(), 2);
	assert_eq!(result, 4);
    }

    #[test]
    fn test2() {
	let result = Solution::character_replacement("AABABBA".to_string(), 1);
	assert_eq!(result, 4);
    }

    #[test]
    fn test3() {
	let result = Solution::character_replacement("CCBBBB".to_string(), 2);
	assert_eq!(result, 6);
    }

    #[test]
    fn test4() {
	let result = Solution::character_replacement("ABAB".to_string(), 0);
	assert_eq!(result, 1);
    }
}


fn main() {

}
