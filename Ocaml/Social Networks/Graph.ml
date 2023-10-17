module Graph : GraphAbs =
  struct
    
    (* definito il tipo 'a graph *)
    type 'a graph = Empty | Graph of 'a list * ('a * 'a) list

    exception Empty_graph
    exception Node_not_present

    (* restituisce un grafo vuoto *)
    let create () = Empty

    (* funzione non accessibile all'esterno del modulo *)
    (* restituisce una lista di tipo lst@[element] se element non è ancora presente nella lista *)
    let rec insert_in_list element lst =
      match lst with
      | []   -> [element]
      | h::t -> if h = element then t else h::insert_in_list element t
    
    (* funzione non accessibile all'esterno del modulo *)
    (* restituisce una lista senza l'elemento element da lst *)
    let rec delete_in_list element lst =
      match lst with
      | []   -> []
      | h::t -> if h = element then t else h::delete_in_list element lst
    
    (* funzione non accessibile all'esterno del modulo *)
    (* restituisce true se element è nella list, false altrimenti *)
    let is_in element lst = List.exists (fun x -> x = element) lst

    (* restituisce un grafo dove viene aggiunto un nuovo nodo se questo non è già presente *)
    let add_node graph node =
      match graph with
      | Empty -> Graph( [node], [] )
      | Graph(nodes, edges) -> Graph(insert_in_list node nodes, edges)

    (* questa funzione aggiunge un arco tra node e node' se questo non è già presente, se i due nodi non esistono solleva Node_not_present *)
    (* se il grafo è vuoto solleva Empty_exception *)
    let add_edge graph node node' =
      match graph with
      | Empty -> raise Empty_graph
      | Graph(nodes, _) when (not (is_in node nodes)) || (not (is_in node' nodes)) -> raise Node_not_present
      | Graph(nodes, edges) -> Graph(nodes, insert_in_list (node, node') edges) 
    
    (* restituisce un grafo dove a partire da graph viene eliminato il nodo node se presente, e tutti gli archi in cui è presente *)
    let remove_node graph node =
      match graph with
      | Empty -> Empty
      | Graph(nodes, edges) -> Graph(delete_in_list node nodes, List.filter (fun x -> not ((fst x) = node || (snd x) = node)) edges)
    
    (* restituisce un grafo uguale a graph senza l'arco (node, node') se questo è presente *)
    let remove_edge graph node node' =
      match graph with
      | Empty -> Empty
      | Graph(nodes, edges) -> Graph(nodes, delete_in_list (node, node') edges)
    
    (* restituisce una lista di nodi node' t.c. esiste l'arco (node, node') *)
    let successors graph node =
      match graph with
      | Empty -> raise Empty_graph
      | Graph(nodes, _) when not (is_in node nodes) -> raise Node_not_present
      | Graph(nodes, edges)  -> let rec successors lst =
                                  match lst with
                                  | [] -> []
                                  | (_, node')::t -> node'::successors t
                                in successors (List.filter (fun x -> (fst x) = node) edges)
    
    (* restituisce una lista di nodi node' t.c. esiste l'arco (node', node) *)
    let predecessors graph node = 
      match graph with
      | Empty -> raise Empty_graph
      | Graph(nodes, _) when not(is_in node nodes) -> raise Node_not_present
      | Graph(nodes, edges)  -> let rec  predecessors lst =
                                  match lst with
                                  | [] -> []
                                  | (node', _)::t -> node'::predecessors t
                                in predecessors (List.filter (fun x -> (snd x) = node) edges)

  end;;