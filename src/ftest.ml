open Gfile
open Tools
open Fordfulk
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let grint = (gmap graph (fun x -> int_of_string(x))) in
  let g1 = clone_nodes graph in
  let g2 = gmap graph (fun x -> x ^ "toto" ) in
  let g3 = gmap (add_arc grint 0 2 (-8)) (fun x -> string_of_int(x)) in
  (*let g4 = find_path grint source sink in (* find path *)*)
  let g4 = find_path2 grint source sink in (* find_path2 avec sécurité *)
  let g5 = capacite_min grint g4 in
  let g6 = maj_graph grint g5 g4 in
  let g7 = ford_fulkerson grint source sink in

  
  (* Rewrite the graph that has been read. *)

  
 (*let () = Printf.printf "capacite : %d \n" g5 in
  let () = List.iter (Printf.printf "%d ") g4 in*) 
  
  (*
  let () = write_file outfile g3 in
  *)
  let () = export outfile (gmap g7 (fun x -> string_of_int(x))) in
  

  ()