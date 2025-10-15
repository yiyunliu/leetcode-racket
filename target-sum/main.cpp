#include<unordered_map>
#include<utility>
#include<vector>
using std::vector;


using mysize = std::vector<int>::size_type;

struct pair_hash {
    template <class T1, class T2>
    std::size_t operator () (const std::pair<T1,T2> &p) const {
	auto h1 = std::hash<T1>{}(p.first);
	auto h2 = std::hash<T2>{}(p.second);

	// Mainly for demonstration purposes, i.e. works but is overly simple
	// In the real world, use sth. like boost.hash_combine
	return h1 ^ h2;
    }
};

int loop(std::unordered_map<std::pair<mysize, int>,int,pair_hash>&cache, const vector<int>& nums, mysize i, int target) {
  if (i == 0) {
    return target == 0 ? 1 : 0;
  }

  const auto cache_ref = [&cache, &nums](mysize i, int target) {
    const std::pair a{i,target};
    if (cache.contains(a)) {
      return cache[a];
    }
    const auto result = loop(cache,nums,i,target);
    cache[a] = result;
    return result;
  };

  const auto num = nums[i-1];
  return cache_ref(i-1,target - num) + cache_ref(i-1,target+num);
}

int findTargetSumWays(vector<int>& nums, int target) {
  std::unordered_map<std::pair<mysize, int>,int,pair_hash> cache;
  return loop(cache,nums,nums.size(),target);
}
