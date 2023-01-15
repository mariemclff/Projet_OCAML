open Graph
open Tools

exception Impossible
exception NoeudInexistant

 (* find_path avec une sécurité quand le label est à 0, pour ne plus considérer ce chemin*)
 (* but : trouver chemin entre source et puits *)
 (* parametres : src = source puis noeud actuel ; chemin = acu (liste des noeuds en construction entre source et puits) ; 
    arcs = list d'arcs suivant le noeud actuel *)
 let find_path2 (gr: int graph) source puits =
  let rec aux src arcs chemin = (**)
    if List.mem src chemin then [] (* vérification qu'on ne revient pas sur un noeud déjà traversé => risque de boucle infinie*)
    else 
      match find_arc gr src puits with
      | None | Some 0 -> (* si l'arc entre le noeud actuel et le puits a un label de 0, on l'ignore *)
        begin
        match arcs with 
        | [] -> []
        | (id, 0) :: rest -> aux src rest chemin (* si le label est à 0, on l'ignore et on passe à l'arc suivant *)
        | (id, lbl) :: rest -> 
          let next_arcs = out_arcs gr id in (* on regarde la liste des arcs sortants du noeud id *)
          let new_chemin = src :: chemin in (* on ajoute src à la liste des noeuds empruntés *)
          let result = aux id next_arcs new_chemin in (* on appelle la fonction récursive avec le nouveau noeud *)
          if result = [] then
            aux src rest chemin (* si le nouveau noeud ne mène à rien, on prend un nouvel arc du noeud précédent*)
          else result
        end
      | Some lbl -> puits :: src :: chemin (* si le prochain arc est lié au puits,c'est terminé *)

      in
  let arcs_source = out_arcs gr source in
  
  if (node_exists gr source && node_exists gr puits)
    then List.rev (aux source arcs_source [])
  else raise NoeudInexistant

  (* but : créer un accumulateur qui retient la capacité minimale rencontrée sur le chemin entre la source et le puits *)
  (* parametres : graph, chemin : liste des noeuds entre la source et le puits *)
let capacite_min graph chemin =
  let rec aux acu noeuds =
    match noeuds with
    | [] -> acu (* pas de noeuds dans la liste, le chemin est vide *)
    | id :: [] -> acu (* on est arrivé au noeud puits *)
    | id1 :: id2 :: rest ->
      match find_arc graph id1 id2 with
      | Some lbl -> if lbl < acu then aux lbl (id2 :: rest) else aux acu (id2 :: rest) (* si le label trouvé entre le noeud actuel et le suivant est inférieur à notre accumulateur, on remplace sa valeur par le label*)
      | None -> acu
      in
      aux max_int chemin

(* but : parcourir chemin (sens source -> puits) et soustraire flot sur chaque arc du chemin *)
(*  et : parcourir chemin (sens puits -> source) et ajouter flot sur chaque arc du chemin *)
(* parametres : graph, flot : resultat de capacite_min, chemin : liste des noeuds entre la source et le puits *)
let maj_graph graph flot chemin =
(* parametres : noeuds = liste des noeuds composant le chemin, acugraph : graphe modifié *)
  let rec soustraire noeuds acugraph =
    match noeuds with
    | [] -> acugraph (* pas de noeuds dans la liste, le chemin est vide *)
    | id :: [] -> acugraph (* il ne reste plus de noeuds dans le chemin, tout le graphe a été modifié *)
    | id1 :: id2 :: rest -> soustraire (id2 :: rest) (add_arc acugraph id1 id2 (-flot)) 
    in
      
  let g1 = soustraire chemin graph in
(* parametres : noeuds = liste des noeuds composant le chemin, acugraph : graphe modifié *)        
  let rec ajouter noeuds acugraph = 
    match noeuds with
    |[] -> acugraph (* pas de noeuds dans la liste, le chemin est vide *)
    | id :: [] -> acugraph (* il ne reste plus de noeuds dans le chemin, tout le graphe a été modifié *)
    | id1 :: id2 :: rest -> ajouter (id2 :: rest) (add_arc acugraph id1 id2 flot) 
  in
  let chemin_reverse = List.rev(chemin) in (* on retourne la liste des noeuds traversés pour pouvoir ajouter les arcs inverses *)
  ajouter chemin_reverse g1

  (* but : effectuer toutes les itérations de l'algo de ford fulkerson jusqu'à ce qu'on ne puisse plus trouver de chemin empruntable *)
  (* parametres : graph, source : noeud source, puits : noeud puits *)
let ford_fulkerson gr source puits = 
  let rec aux graph debit_max =
    let chemin = find_path2 graph source puits in
    match chemin with
    | [] -> 
      Printf.printf "flot max atteint : %d \n" debit_max ; 
      graph (* pas ou plus de chemin, on renvoie le graphe et on affiche le débit maximal trouvé *)
    | noeud :: rest -> 
          let flot = capacite_min graph chemin in
          Printf.printf "nouvelle itération, flot : %d%!\n" flot ;
          let new_graph = maj_graph graph flot chemin in
          aux new_graph (debit_max + flot)
    in
  aux gr 0 (* debit max initial à 0, puis s'incrémente avec le flot de chaque itération *)

    (* but : reprendre le graphe d'origine, en mettant à jour les labels de ses arcs avec les résultats de Ford Fulkerson *)
    (* parametres : gr : le graphe d'origine, grff : le graphe d'écart en sortie de Ford Fulkerson *)
let finalgraph gr grff =
  (* parametres : praph : graphe d'origine, id1, id2 : id de noeuds, label : label d'un arc du graphe d'origine *)
  let maj_arc graph id1 id2 label = 
    match find_arc grff id1 id2 with
    | None -> raise Impossible (* si l'arc du graphe initial n'est pas dans le graphe d'écart en sortie de Ford Fulkerson (ce qui est impossible car on ne peut pas supprimer d'arc)*)
    | Some lbl -> new_arc graph id1 id2 (label-lbl) (* on soustrait la valeur de l'arc du graphe d'origine (label) par la valeur du flot max
       parcourant cet arc en sortie de Ford Fulkerson (lbl) pour obtenir la valeur de la capacité restante *)

  in
  e_fold gr maj_arc (clone_nodes gr) (* on applique la fonction maj_arc sur tous les arcs du graph initial (gr) *)
  