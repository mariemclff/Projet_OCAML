open Graph
open Printf
    
type path = string


(* Compute arbitrary position for a node. Center is 300,300 *)
let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100
  

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count id1 id2 lbl -> fprintf ff "e %d %d %d %s\n" id1 id2 count lbl ; count + 1) 0 in
  
  fprintf ff "\n%% End of graph\n" ;
  
  close_out ff ;
  ()
  
(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with a node. *)
let read_guest id dico line =
  try Scanf.sscanf line "g %s %[^\n]" (fun name contraintes -> List.append dico [(id, name, (String.split_on_char ' ' contraintes))])
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an arc. *)
let read_host id dico line =
  try Scanf.sscanf line "h %s %d %[^\n]" (fun name capacite contraintes -> List.append dico [(id, name, capacite, (String.split_on_char ' ' contraintes))])
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line dico_guest dico_host =
  try Scanf.sscanf line " %%" (graph, dico_guest, dico_host)
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"



(* Création des arcs *)

let arcs_source_guests graph dico_guest =
  (* ajout des arcs entre la source et tous les noeuds guests avec une capacité de 1*)
  let rec aux graph dico_guest =
    match dico_guest with
    |[] -> graph
    |(id, name, constraints) :: rest -> 
      let new_graph = new_arc graph 0 id 1 in
      aux new_graph rest
  in  
  aux graph dico_guest

let arcs_hosts_puits graph dico_host id_puits =
  (* ajout des arcs entre tous les noeuds hosts et le puits, avec une capacité égale au nombre de guests qu'ils peuvent accueillir*)
  let rec aux graph dico_host =
    match dico_host with
    |[] -> graph
    |(id, name, capacity, constraints) :: rest ->
      let new_graph = new_arc graph id id_puits capacity in
      aux new_graph rest
    in
  aux graph dico_host


(* ajout des arcs entre les noeuds guests et les noeuds hosts en fonction des contraintes *)

let arcs_guests_hosts graph dico_guest dico_host =
  (* première boucle sur les hosts, on parcourt le dico un par un *)
  let rec aux1 graph dico_host = 
    match dico_host with
    |[] -> graph
    |(id_h, name_h, capa_h, con_h) :: rest1 ->   
    (* deuxième boucle : pour chaque host, on fait une boucle sur le dico des guests*)
    let rec aux2 graph dico_guest con_h = 
      match dico_guest with
      |[] -> (* on est arrivé au bout de la liste des guests ou bien celle ci est vide,
         on passe donc aux hôtes suivants *)
         aux1 graph rest1
      |(id_g, name_g, con_g) :: rest2 -> 
      (* troisième boucle : pour chaque guest on parcourt sa liste de contraintes
         et pour chaque contrainte on fait un List.mem sur la liste de contraintes du host*)
         let rec aux3 graph con_g con_h =
          match con_g with
          |[] -> (* pas ou plus de contrainte, guest accepté partout, on peut le relier par un arc de capacité 1 à cet hôte càd id_h *)
            let new_graph = new_arc graph id_g id_h 1 in
            (* et on continue à regarder pour les autres guests*)
            aux2 new_graph rest2 con_h
          | contrainte :: rest -> 
            match List.mem contrainte con_h with
            | true -> (* ce guest ne peut pas être accueilli par cet hôte *)
                (* et on continue à regarder pour les autres guests *)
                aux2 graph rest2 con_h
            | false -> 
              (* on vérifie que le reste des contraintes est bon aussi*)
              aux3 graph rest con_h
          in
          aux3 graph con_g con_h
      in
      aux2 graph dico_guest con_h
  in
  aux1 graph dico_host

  (* ÉCRIRE UNE FONCTION POUR PRINT LA LIST DES GUESTS ET DES HOSTS AVEC LEURS ID *)
let print_dico dico_guest dico_host =
  let rec aux_g dico_guest = 
    match dico_guest with
    |[] -> Printf.printf " \n"
    |(id, name, _contraintes) :: rest -> 
      Printf.printf "%d " id ; 
      Printf.printf "%s \n" name ; 
      aux_g rest
    in 
  let rec aux_h dico_host = 
    match dico_host with
    |[] -> Printf.printf " \n " 
    |(id, name, _capa, _contraites) :: rest -> 
      Printf.printf "%d " id ; 
      Printf.printf "%s \n" name ; 
      aux_h rest
  in
  Printf.printf "Liste des guests :\n" ;
  aux_g dico_guest ;
  Printf.printf "Liste des hosts :\n" ;
  aux_h dico_host


let from_file_medium path =

(* Création des noeuds guests et hosts *)

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph id dico_guest dico_host =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (graph, dico_guest, dico_host) =
        (* Ignore empty lines *)
        (* si ligne vide, on ne modifie ni le graphe ni les dico*)
        if line = "" then (graph, dico_guest, dico_host)

        (* The first character of a line determines its content : g for guest or e h for host. *)
        else match line.[0] with
          (* si on a un guest, on ajoute un noeud correpondant dans le graph
             puis on l'ajoute au dictionnaire càd la liste de tous les guest avec leur id associé
             et la liste des host reste inchangée *)
          | 'g' -> ((new_node graph id), (read_guest id dico_guest line), dico_host)
          (* si on a un host, on ajoute un noeud correpondant dans le graph
             puis on l'ajoute au dictionnaire càd la liste de tous les host avec leur id associé
             et la liste des guest reste inchangée *)
          | 'h' -> ((new_node graph id), dico_guest, (read_host id dico_host line))

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line dico_guest dico_host
      in 
      loop graph (id+1) dico_guest dico_host

    with End_of_file -> (graph, id, dico_guest, dico_host) (* Done *)
  in

  let (final_graph, id_last, dico_guest, dico_host) = loop empty_graph (-1) [] [] in
  
  close_in infile ;

  print_dico dico_guest dico_host ;

  let id_puits = id_last in

  Printf.printf "id_puits = %d \n" id_puits ;
  
(* Ajout du noeud source et du noeud puits*)
  let final_graph = new_node final_graph 0 in
  let final_graph = new_node final_graph id_puits in

  (* ajout des arcs à final_graph *)
  let final_graph = arcs_source_guests final_graph dico_guest in
  let final_graph = arcs_hosts_puits final_graph dico_host id_puits in
  let final_graph = arcs_guests_hosts final_graph dico_guest dico_host in

  final_graph  

let export path graph =

    (* Open a write-file. *)
    let ff = open_out path in
  
    (* Write in this file. *)
    fprintf ff "digraph finite_state_machine {
      fontname=\"Helvetica,Arial,sans-serif\"
      node [fontname=\"Helvetica,Arial,sans-serif\"]
      edge [fontname=\"Helvetica,Arial,sans-serif\"]
      rankdir=LR;
      node [shape = circle];\n" ;
  
    (* Write all arcs *)
    e_iter graph (fun id1 id2 lbl -> fprintf ff "%d -> %d  [label = \"%s\"];\n" id1 id2 lbl) ;
  
    fprintf ff " }\n" ;
  
    close_out ff ;
    ()
