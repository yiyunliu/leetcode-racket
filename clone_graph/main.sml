datatype node = Node of {value : int,  neighbors : node list}

fun cloneGraph (Node{value, neighbors}) =
    Node {value = value, neighbors = List.map cloneGraph neighbors}
