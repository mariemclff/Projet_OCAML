open Graph
open Tools

exception Impossible
exception NoeudInexistant

 (* find_path avec une sécurité quand le label est à 0, pour ne plus considérer ce chemin*)
 (* but : trouver chemin entre source et puits *)
 let find_path2 (gr: int graph) source puits =
  (* src = source puis noeud actuel ; chemin = acu ; arcs = list d'arcs suivant le noeud actuel *)
  let rec aux src arcs chemin = (**)
    if List.mem src chemin then [] (* vérification qu'on ne revient pas sur un noeud déjà traversé => risque de boucle infinie*)
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
      | Some lbl -> puits :: src :: chemin (* si le prochain arc est lié au puits,c'est terminé *)

      in
  let arcs_source = out_arcs gr source in
  
  if (node_exists gr source && node_exists gr puits)
    then List.rev (aux source arcs_source [])
  else raise NoeudInexistant

  (* graph : graphe en entrée, chemin : liste des noeuds entre la source et le puits *)
  (* but : créer un accumulateur qui retient la capacité minimale rencontrée sur le chemin entre la source et le puits *)
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
      aux max_int chemin^

(* but : parcourir chemin (sens source -> puits) et soustraire flot sur chaque arc du chemin *)
(*  et : parcourir chemin (sens puits -> source) et ajouter flot sur chaque arc du chemin *)
(* parametres : graph : le graphe, flot : resultat de capacite_min, chemin : liste des noeuds entre la source et le puits *)
let maj_graph graph flot chemin =
(* noeuds = liste des noeuds composant le chemin *)
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

  (* but : effectuer toutes les itérations de l'algo de ford fulkerson jusqu'à ce qu'on puisse plus trouver de chemin empruntable *)
  (* parametres : graph : le graphe, source : noeud source, puits : noeud puits *)
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
  