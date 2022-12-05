open Graph

let find_path (gr: int graph) source puit =
  (* id = source puis noeud actuel ; chemin = acu ; arcs = list d'arcs suivant le noeud actuel*)
  let rec aux id arcs chemin =
    match find_arc gr id puit with
    | Some lbl -> puit :: chemin
    | None -> 
      match arcs with 
      (* problème à régler si on est sur un noeud sans successeurs mais qu'il existe une autre solution *)
      | [] -> []
      | (p, _) :: rest -> if p = puit then puit :: chemin
      | (id, lbl) :: rest -> 
        if aux id (out_arcs gr id) (id :: chemin) = [] 
          then aux id rest (id :: chemin) 



  let arcs_source = out_arcs gr source in

  List.rev (aux source arcs_source [source])
