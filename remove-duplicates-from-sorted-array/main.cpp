class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
	auto iter_uniq = std::begin(nums);
	auto iter_next = std::begin(nums);
	while(true) {
	    const auto num = *iter_uniq;
	    while(true) {
		if (iter_next == std::end(nums)) {
		    return 1 + (iter_uniq - std::begin(nums));
		}
		if (*iter_next != num) {
		    break;
		}
		++iter_next;
	    }
	    ++ iter_uniq;
	    std::swap(*iter_uniq, *iter_next++);
	}
    }
};
