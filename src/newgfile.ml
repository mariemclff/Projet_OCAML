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
  try Scanf.sscanf line "g %s [%[^\n]" (fun name contraintes -> List.append dico [(id, name, cpacite, (String.split_on_char ' ' contraintes))])
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an arc. *)
let read_host id dico line =
  try Scanf.sscanf line "h %s %d [%[^\n]" (fun name capacite contraintes -> List.append dico [(id, name, (String.split_on_char ' ' contraintes))])
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line dico_guest dico_host =
  try Scanf.sscanf line " %%" (graph, dico_guest, dico_host)
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"





let from_file_medium path =

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
          | 'g' -> ((new_node graph id), (read_guest line id dico_guest), dico_host)
          (* si on a un host, on ajoute un noeud correpondant dans le graph
             puis on l'ajoute au dictionnaire càd la liste de tous les host avec leur id associé
             et la liste des guest reste inchangée *)
          | 'h' -> ((new_node graph id), dico_guest, (read_host line id dico_host))

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line dico_guest dico_host
      in 
      loop graph (id+1) dico_guest dico_host

    with End_of_file -> (graph, id, dico_guest, dico_host) (* Done *)
  in

  let (final_graph, id_last, dico_guest, dico_host) = loop empty_graph 0 [] [] in
  
  close_in infile ;

(* TESTS À ENLEVER *)
  let rec print_guest list = match list with
    |(id, name, list)::rest -> Printf.printf "%d " id; Printf.printf "%s \n" name; print_guest rest
    |[] -> Printf.printf "%s" "fin"
  in

  let rec print_host list = match list with
    |(id, name, nb_place, _)::rest -> Printf.printf "%d " id; Printf.printf "%d " nb_place; Printf.printf "%s \n" name; print_host rest
    |[] -> Printf.printf "%s" "fin"
  in

  print_guest list_guest;
  print_host list_host;

  (* FIN TESTS *)


(* Ajout du noeud source et du noeud puits*) (* a vérifier que les id des noeuds sont bons au moment de faire le graph*)
  let final_graph = new_node final_graph 0 in
  let final_graph = new_node final_graph id_fin in

(* fonction des trois boules *)

  (* première boucle sur les hosts, on parcourt le dico un par un *)
    (* deuxième boucle : pour chaque host, on fait une boucle sur le dico des guests*)
      (* troisième boucle : pour chaque guest on parcourt sa liste de contraintes
         et pour chaque contrainte on fait un List.mem sur la liste de contraintes du host*)







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
