open Graph

let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph

let gmap (gr:'a graph) (f:'a -> 'b) = 
  e_fold gr (fun acu id1 id2 lbl -> new_arc acu id1 id2 (f lbl)) (clone_nodes gr) 

let add_arc g id1 id2 n = 
  match (find_arc g id1 id2) with
  | None -> new_arc g id1 id2 n
  | Some arc ->  new_arc g id1 id2 (arc+n)
