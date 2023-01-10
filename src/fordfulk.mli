open Graph

(*val find_path: int graph -> id -> id -> id list*)
val capacite_min: int graph -> id list -> int
val maj_graph : int graph -> int -> id list -> int graph
val find_path2: int graph -> id -> id -> id list
val ford_fulkerson: int graph -> id -> id -> int graph (* /!\ à changer si on renvoie aussi le debit max *)