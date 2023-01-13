open Graph

(* returns a new graph having the same nodes than gr, but no arc *)
let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph

(* maps all arcs of gr by funciton f *)
let gmap (gr:'a graph) (f:'a -> 'b) = 
  e_fold gr (fun acu id1 id2 lbl -> new_arc acu id1 id2 (f lbl)) (clone_nodes gr) 

(* adds n to the value of the arc between id1 and id2. If the arc does not exists, it is created. *)
let add_arc g id1 id2 n = 
  match (find_arc g id1 id2) with
  | None -> new_arc g id1 id2 n
  | Some arc ->  new_arc g id1 id2 (arc+n)
