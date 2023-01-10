open Graph
open Tools

(*let find_path (gr: int graph) source puits =
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

  List.rev (aux source arcs_source [source])*)

exception Impossible

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
      aux max_int chemin


let maj_graph graph flot chemin =
(* parcourir chemin et soustraire flot sur chaque arc du chemin *)
(* noeuds c'est chemin *)
  let rec soustraire noeuds acugraph =
    match noeuds with
    | [] -> acugraph
    | id :: [] -> acugraph
    | id1 :: id2 :: rest -> soustraire (id2 :: rest) (add_arc acugraph id1 id2 (-flot)) 
    in
      
  let g1 = soustraire chemin graph in
        
  let rec ajouter noeuds acugraph = 
    match noeuds with
    |[] -> acugraph
    | id :: [] -> acugraph
    | id1 :: id2 :: rest -> ajouter (id2 :: rest) (add_arc acugraph id1 id2 flot) 
  in
  let chemin_reverse = List.rev(chemin) in
  ajouter chemin_reverse g1

  (* test de créer un find_path avec une sécurité quand le label est à 0, pour ne plus considérer ce chemin*)
let find_path2 (gr: int graph) source puits =
  (* src = source puis noeud actuel ; chemin = acu ; arcs = list d'arcs suivant le noeud actuel*)
  let rec aux src arcs chemin =
    if List.mem src chemin then []
    else 
      match find_arc gr src puits with
      | None | Some 0 -> 
        begin
        match arcs with 
        | [] -> []
        | (id, 0) :: rest -> aux src rest chemin
        | (id, lbl) :: rest -> 
          let next_arcs = out_arcs gr id in
          let new_chemin = src :: chemin in
          let result = aux id next_arcs new_chemin in
          if result = [] then
            aux src rest chemin
          else result
        end
      | Some lbl -> puits :: src :: chemin

      in
  let arcs_source = out_arcs gr source in
  
  List.rev (aux source arcs_source [])

let ford_fulkerson gr source puits = 
  let rec aux graph debit_max =
    let chemin = find_path2 graph source puits in
    match chemin with
    | [] -> graph (* pas ou plus de chemin, on renvoie le graphe (on peut aussi renvoyer le debut_max)*)
    | noeud :: rest -> 
          let flot = capacite_min graph chemin in
          Printf.printf "flot : %d%!\n" flot ;
          let new_graph = maj_graph graph flot chemin in
          aux new_graph (debit_max + flot)
    in
  aux gr 0

let finalgraph gr grff =
(* à faire pour chaque arc de gr => récupérer son id1 et id2*)
  let maj_arc graph id1 id2 label = 
    match find_arc grff id1 id2 with
    | None -> raise Impossible
    | Some lbl -> new_arc graph id1 id2 (label-lbl)

  in
  e_fold gr maj_arc (clone_nodes gr)
  
  (*let new_gr = clone_nodes gr in
  let rec aux new_gr src =
    match out_arcs gr src with
    | [] -> (*trouver quoi faire*)
    | (id, lbl) :: rest -> 
      match find_arc id src with
      | None -> new_arc new_gr src id lbl 
      | Some lbl -> (*trouver quoi faire*)
  (*out_arcs de la source -> on les ajoute dans le nv graph*)

  (* puis pour tous les (id, lbl) dans out graphs, on fait ses out_arcs et on les ajoute dans le grpahe à part pour la src du id*)

  (*let rec loop graph = match (depthSearch graph src puits) with
    |(a::tail) as liste -> loop (updateFlow graph liste)
    |[] -> graph
  in loop gr*)
  *)
