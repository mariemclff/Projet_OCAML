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
    | [] -> acu
    | id :: [] -> acu
    | id1 :: id2 :: rest ->
      match find_arc graph id1 id2 with
      | Some lbl -> if lbl < acu then aux lbl (id2 :: rest) else aux acu (id2 :: rest)
      | None -> acu
      in
  aux max_int chemin ;
  (*tester capacite_min*)

  (*
let maj_graph graph flot chemin =
  (* parcourir chemin et soustraire flot sur chaque arc du chemin *)
  let rec soustraire noeuds =
    match noeuds with
    | [] -> graph
    | id :: [] -> acu
    | id1 :: id2 :: (* qqch ???? *)
  in
  let g1 = gmap graph soustraire in
  (* parcourir chemin inverse (List.reverse) et ajouter flot sur arc inverse avec add_arc, qui crée l'arc s'il n'existe pas *)
  (* return un nouveau graphe d'état mis à jour*)
  *)