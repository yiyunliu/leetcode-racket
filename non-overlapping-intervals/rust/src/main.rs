pub struct Solution;

impl Solution {
    pub fn erase_overlap_intervals(mut intervals: Vec<Vec<i32>>) -> i32 {
	intervals.sort_unstable_by_key(|v| v[0]);
	let mut iter = intervals.iter();
	match iter.next() {
	    None => 0,
	    Some(first_interval) => {
		let mut end = first_interval[1];
		let mut acc = 0;
		for interval in iter {
		    if interval[0] < end {
			acc += 1;
			end = std::cmp::min(interval[1],end);
		    }
		    else {
			end = interval[1];
		    }
		}
		acc
	    },
	}

    }
}

#[cfg(test)]
mod tests {
    use super::Solution;
    #[test]
    fn test0 () {
	let result = Solution::erase_overlap_intervals(vec![vec![1,2],vec![2,3],vec![3,4],vec![1,3]]);
	assert_eq!(result, 1);
    }

    #[test]
    fn test1 () {
	let result = Solution::erase_overlap_intervals(vec![vec![1,2],vec![1,2],vec![1,2]]);
	assert_eq!(result, 2);
    }

    #[test]
    fn test2 () {
	let result = Solution::erase_overlap_intervals(vec![vec![1,2],vec![2,3]]);
	assert_eq!(result, 0);
    }

    #[test]
    fn test3 () {
	let result = Solution::erase_overlap_intervals(vec![vec![1,100],vec![11,22],vec![1,11],vec![2,12]]);
	assert_eq!(result, 2);
    }

}


fn main() {
    println!("Hello, world!");
}
