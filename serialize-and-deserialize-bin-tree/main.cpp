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
    std::string serialize(TreeNode* root) {
    std::string s;
    std::stack<TreeNode*> stack;
    stack.push(root);
    while (!stack.empty()) {
      auto node = stack.top();
      stack.pop();
      if (node) {
	s.append(std::to_string(node->val));
	s.push_back(' ');
	stack.push(node->right);
	stack.push(node->left);
      }
      else {
	s.append("1001 ");
      }
    }
    return s;
  }

  // Decodes your encoded data to tree.
  TreeNode* deserialize(std::string data) {
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
