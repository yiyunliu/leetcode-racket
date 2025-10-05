use std::collections::HashMap as Map;
use std::collections::HashSet as Set;

pub struct Solution;

impl Solution {
    pub fn min_window(s: String, t: String) -> String {
        let s : &[u8] = s.as_bytes();
	let mut target_counts : Map<u8,usize> = Map::new();
	let mut counts : Map<u8,usize> = Map::new();
	let mut targets_unmet : Set<u8> = Set::new();
	for &ch in t.as_bytes().iter() {
	    targets_unmet.insert(ch);
	    *target_counts.entry(ch).or_insert(0) += 1;
	    counts.insert(ch,0);
	}

	let target_counts = target_counts;
	let mut init_idx = None;

	for (idx, ch) in s.iter().enumerate() {
	    if let Some(count) = counts.get_mut(ch) {
		*count += 1;
		if count == target_counts.get(ch).unwrap() {
		    targets_unmet.remove(ch);
		}
	    }
	    if targets_unmet.is_empty() {
		init_idx = Some(idx+1);
		break;
	    }
	}
	// dbg!(init_idx);


	match init_idx {
	    None => {String::from("")},
	    Some(init_idx) => {
		// dbg!(std::str::from_utf8(&s[0..init_idx]).unwrap().to_string());
		let mut start = 0;
		let mut end = init_idx;
		let mut best = &s[start..end];
		loop {
		    while start < end {
			let start_ch = &s[start];
			let start_count = counts.get_mut(start_ch);
			let should_increment =
			    match start_count {
				Some(start_count) => {
				    let result = *start_count > target_counts[start_ch];
				    if result {
					*start_count -= 1;
				    }
				    result
				},
				None => true
			    };
			if should_increment {
			    start += 1;
			}
			else {
			    break;
			}
		    }

		    best = std::cmp::min_by_key(best,&s[start..end],|x| x.len());

		    if end >= s.len() {
			break;
		    }

		    let end_ch = &s[end];
		    if let Some(count) = counts.get_mut(end_ch) {
			*count += 1;
		    }
		    end += 1;
		}
		std::str::from_utf8(best).unwrap().to_string()
	    }
	}
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::min_window("ADOBECODEBANC".to_string(),"ABC".to_string()),"BANC");
	assert_eq!(Solution::min_window("a".to_string(),"a".to_string()),"a");
	assert_eq!(Solution::min_window("a".to_string(),"aa".to_string()),"");
	assert_eq!(Solution::min_window("ab".to_string(),"a".to_string()),"a");
	assert_eq!(Solution::min_window("ab".to_string(),"b".to_string()),"b");
	assert_eq!(Solution::min_window("abc".to_string(),"b".to_string()),"b");
    }
}
