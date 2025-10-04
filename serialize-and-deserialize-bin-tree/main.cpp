#include<sstream>
#include<stack>


struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
};

class Codec {
public:

    // Encodes a tree to a single string.
    string serialize(TreeNode* root) {
	std::ostringstream s;
	std::stack<TreeNode*> stack;
	stack.push(root);
	while (!stack.empty()) {
	    auto node = stack.top();
	    stack.pop();
	    if (node) {
		s << node -> val <<" ";
		stack.push(node);
	    }
	    else {
		s << 1001;
	    }
	}
	return s.str();
    }

    // Decodes your encoded data to tree.
    TreeNode* deserialize(string data) {
	std::istringstream s{data};
	return deserialize(s);
    }

    TreeNode* deserialize(std::istringstream &s) {
	int i;
	s >> i;
	if (i == 1001) return nullptr;

	const auto left = deserialize(s);
	const auto right = deserialize(s);
	return new TreeNode {i, left, right};

    }
};
