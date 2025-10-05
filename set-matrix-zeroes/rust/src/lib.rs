pub struct Solution;

impl Solution {
    pub fn set_zeroes(matrix: &mut Vec<Vec<i32>>) {
	let nrows = matrix.len();
	let ncols = matrix[0].len();
	let mut row_clear = vec![false; nrows];
	let mut col_clear = vec![false; ncols];
	for i in 0 .. nrows {
	    for j in 0 .. ncols {
		if matrix[i][j] == 0 {
		    row_clear[i] = true;
		    col_clear[j] = true;
		}
	    }
	}
	for i in (0 .. nrows).filter(|i| row_clear[*i]) {
	    for j in 0 .. ncols {
		matrix[i][j] = 0;
	    }
	}
	for j in (0 .. ncols).filter(|i| col_clear[*i]) {
	    for i in 0..nrows {
		matrix[i][j] = 0;
	    }
	}
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut result = vec![vec![0,1,2,0],vec![3,4,5,2],vec![1,3,1,5]];
	Solution::set_zeroes(&mut result);
	let expected = vec![vec![0,0,0,0],vec![0,4,5,0],vec![0,3,1,0]];
        assert_eq!(result, expected);
    }

    #[test]
    fn it_works_too() {
        let mut result = vec![vec![1,1,1],vec![1,0,1],vec![1,1,1]];
	Solution::set_zeroes(&mut result);
	let expected = vec![vec![1,0,1],vec![0,0,0],vec![1,0,1]];
        assert_eq!(result, expected);
    }
    
}
