/*
// Definition for a Node.
class Node {
public:
    int val;
    vector<Node*> neighbors;
    Node() {
	val = 0;
	neighbors = vector<Node*>();
    }
    Node(int _val) {
	val = _val;
	neighbors = vector<Node*>();
    }
    Node(int _val, vector<Node*> _neighbors) {
	val = _val;
	neighbors = _neighbors;
    }
};
*/
#include<vector>

class Solution {
public:
    Node* cloneGraph(Node* node) {
	if (!node) {
	    return nullptr;
	}
	std::vector<Node*> visited = std::vector<Node*>(101);
	Node *curr = new Node();
	cloneGraphRec(*node, *curr, visited);
	return curr;
    }

private:
    void cloneGraphRec(const Node &node, Node &curr, std::vector<Node*> &visited) {
	visited[node.val] = &curr;
	curr.val = node.val;
	const auto &neighbors = node.neighbors;
	const auto sz = neighbors.size();
	curr.neighbors.reserve(sz);
	for (const auto &neighbor : neighbors) {
	    if(!visited[neighbor->val]) {
		auto * new_node = new Node();
		new_node->val = neighbor->val;
		cloneGraphRec(*neighbor, *new_node, visited);
	    }
	    curr.neighbors.push_back(visited[neighbor->val]);
	}
    }
};
