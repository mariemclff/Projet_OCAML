open Graph

let find_path (gr: int graph) source puits =
  (* id = source puis noeud actuel ; chemin = acu ; arcs = list d'arcs suivant le noeud actuel*)
  let rec aux id arcs chemin =
    match find_arc gr id puits with
    | Some lbl -> puits :: chemin
    | None -> 
      match arcs with 
      | [] -> []
      | (id, lbl) :: rest -> 
        let next_arcs = out_arcs gr id in
        let result = aux id next_arcs (id :: chemin) in
        if result = [] then
          aux id rest chemin
        else result 
      in
  let arcs_source = out_arcs gr source in

  List.rev (aux source arcs_source [source])

let capacite_min graph chemin =
  let rec aux acu noeuds =
    match noeuds with
    | [] -> []
    | id1 :: id2 :: rest ->
      match find_arc graph id1 id2 with
      | Some lbl -> if lbl < acu then aux lbl (id2 :: rest) else aux acu (id2 :: rest)
      | None -> acu
      in
  aux 0 chemin ;
  (*tester capacite_min*)

    