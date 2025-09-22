struct Solution;

impl Solution {
    #[inline]
    fn max_option(left: Option<&i32>, right: Option<&i32>) -> i32 {
        *match left {
            None => right.unwrap(),
            Some(l) => match right {
                None => l,
                Some(r) => std::cmp::max(l, r),
            },
        }
    }

    #[inline]
    fn min_option(left: Option<&i32>, right: Option<&i32>) -> i32 {
        *match left {
            None => right.unwrap(),
            Some(l) => match right {
                None => l,
                Some(r) => std::cmp::min(l, r),
            },
        }
    }

    #[inline]
    fn slice_okay(left: &[i32], right: &[i32]) -> bool {
        left.len() == 0 || right.len() == 0 || left.last() <= right.first()
    }

    pub fn find_median_sorted_arrays(nums1_v: Vec<i32>, nums2_v: Vec<i32>) -> f64 {
        let nums1: &[i32];
        let nums2: &[i32];
        if nums1_v.len() < nums2_v.len() {
            nums1 = &nums1_v;
            nums2 = &nums2_v;
        } else {
            nums1 = &nums2_v;
            nums2 = &nums1_v;
        }

        // inner-recursive case
        let iseven = (nums1.len() + nums2.len()) % 2 == 0;
        let half_size = (nums1.len() + nums2.len()) / 2;
        let mut low = 0;
        let mut high = nums1.len();
        loop {
            let idx1 = low + (high - low) / 2;
            let idx2 = half_size - idx1;
            let (slice1l, slice1r) = nums1.split_at(idx1);
            let (slice2l, slice2r) = nums2.split_at(idx2);
            let okay1 = Self::slice_okay(slice1l, slice2r);
            let okay2 = Self::slice_okay(slice2l, slice1r);
            if okay1 {
                if okay2 {
                    let right = Self::min_option(slice1r.first(), slice2r.first()) as f64;
                    if iseven {
                        let left = Self::max_option(slice1l.last(), slice2l.last()) as f64;
                        return (right + left) / 2.0;
                    } else {
                        return right;
                    }
                } else {
                    low = idx1 + 1;
                }
            } else {
                high = idx1;
            }
        }
    }
}

fn main() {
    println!(
        "Result: {}",
        Solution::find_median_sorted_arrays(vec![1, 2, 6, 7, 8], vec![3, 4, 5, 9, 10])
    );
}
