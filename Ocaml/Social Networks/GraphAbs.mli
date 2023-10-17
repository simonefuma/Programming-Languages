module type GraphAbs =
  sig

    type 'a graph

    exception Empty_graph
    exception Node_not_present

    val create : unit -> 'a graph
    val add_node : 'a graph -> 'a -> 'a graph
    val add_edge : 'a graph -> 'a -> 'a -> 'a graph
    val remove_node : 'a graph -> 'a -> 'a graph
    val remove_edge : 'a graph -> 'a -> 'a -> 'a graph
    val successors : 'a graph -> 'a -> 'a list
    val predecessors : 'a graph -> 'a -> 'a list

  end;;