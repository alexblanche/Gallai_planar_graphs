(* Functions related to colors *)

type color = int;;

(* A graph and its coloring *)
(* nc = number of colors used *)
type colored_graph = {cg : graph; cc : coloring; mutable nc : int};;

(* Converts a graph into the type colored_graph *)
let graph_to_colored_graph (g : graph) =
	{cg = g; cc = empty_coloring (number_of_vertices g); nc = 0};;

let get_color (cg : colored_graph) (i : int) (j : int) =
	cg.cc.(i).(j);;

let set_color (cg : colored_graph) (i : int) (j : int) (c : color) =
	cg.cc.(i).(j) <- Some c;
	cg.cc.(j).(i) <- Some c;;

(* Returns a colored that is not used by the colored graph cg, and updates the number of colors used by cg *)
let new_color (cg : colored_graph) =
	let n = cg.nc in
	cg.nc <- n+1;
	n;;

(* Colors with color c all the edges ij encoded as pairs of vertices (i,j) in the list l *)
let color_pair_vertices (cg : colored_graph) (l : (int * int) list) (c : color) =
  List.iter (fun (i,j) -> set_color cg i j c) l;;

(* Returns true if vertex i touches color c in the colored graph cg *)
let touches_color (cg : colored_graph) (i : int) (c : color) =
  let el = present_edges cg.cg i in
  let f e = get_color cg i e.edge_end = Some c in
  List.exists f el;;

(* Build a generic coloring: each edge has a different color *)
let generic_coloring (g : graph) =
	let cg = graph_to_colored_graph g in
	let treat_edge i e =
		if (is_present_edge g i e.edge_end && get_color cg i e.edge_end = None)
			then set_color cg i e.edge_end (new_color cg)
	in
	let rec aux ela i =
		if (i = number_of_vertices g)
			then cg
			else (List.iter (treat_edge i) ela.(i);
					aux ela (i+1))
	in
	aux g.e 0;;

(* generic_coloring g2;; *)
  
(* Greedy algorithm that gives a naive path-coloring of a graph *)
(* Temporary function, only for testing purposes, not to be used in the final program *)
let naive_coloring (cg : colored_graph) (vl : int list) =
  (* Returns true if at least one edge was found *)
  let rec add_path (i : int) (c : color) (b : bool) =
    try
      let f e = ((get_color cg i e.edge_end) = None) && (List.mem e.edge_end vl) && not (touches_color cg e.edge_end c) in
      let j = List.find f (present_edges cg.cg i) in
      set_color cg i j.edge_end c;
      add_path j.edge_end c true
    with
      | Not_found -> b
  in
  (* Returns true if at least one edge was found *)
  let search_twice (i : int) (c : color) =
    match (present_edges cg.cg i) with
    | [] -> false
    | [_] -> add_path i c false
    | e1::e2::_ -> (let _ = add_path i c false in
              add_path i c false)
  in
  (* Applies new colors until all edges incident with i are colored *)
  let rec loop (i : int) (c : color) =
    if search_twice i c
      then loop i (new_color cg)
  in
  (*let rec aux = function
      | [] -> ()
      | i::t -> (loop i (new_color cg); aux t)
  in aux vl;;*)
  List.iter (fun i -> loop i (new_color cg)) vl;;
(* Untested *)

