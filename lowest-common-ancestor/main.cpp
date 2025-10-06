class Solution {
public:
    TreeNode* lowestCommonAncestor(TreeNode* root, const TreeNode* const _p, const TreeNode* const _q) {
	const auto p = _p->val;
	const auto q = _q ->val;
	while(true) {
	    auto val = root->val;
	    if (p > val && q > val) {
		root = root->right;
	    }
	    else if (p < val && q < val) {
		root = root->left;
	    }
	    else {
		return root;
	    }
	}

    }
};
