fn rotate_idx(row : usize, col : usize, size : usize) -> (usize, usize) {
    return (col, size - row -1);
}

fn rotate(matrix: &mut Vec<Vec<i32>>) {
    let size = matrix.len();
    let quad_width = size / 2;
    let quad_height = (size + 1) / 2;
    for row in 0..quad_width {
	for col in 0..quad_height {
	    let tmp = matrix[row][col];
	    let (row0,col0) = rotate_idx(row,col,size);
	    let (row1,col1) = rotate_idx(row0,col0,size);
	    let (row2,col2) = rotate_idx(row1,col1,size);
	    matrix[row][col]=matrix[row2][col2];
	    matrix[row2][col2]=matrix[row1][col1];
	    matrix[row1][col1]=matrix[row0][col0];
	    matrix[row0][col0]=tmp;
	}
    }
}


fn main() {
    println!("Hello, world!");
}
