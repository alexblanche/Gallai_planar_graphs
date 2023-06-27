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
  let rec add_path_aux (b : bool) (i : int) (c : color) =
    try
      let f e = ((get_color cg i e.edge_end) = None) && (List.mem e.edge_end vl) && not (touches_color cg e.edge_end c) in
      let j = List.find f (present_edges cg.cg i) in
      set_color cg i j.edge_end c;
      add_path_aux true j.edge_end c
    with
      | Not_found -> b
  in
  (* Returns true if at least one edge was found *)
  let add_path = add_path_aux false in
  let search_twice (i : int) (c : color) =
    match (present_edges cg.cg i) with
    | [] -> false
    | [_] -> add_path i c
    | e1::e2::_ -> (let _ = add_path i c in
              add_path i c)
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


(* Colors a path with color col *)
let color_path (cg : colored_graph) (p : int list) (col : color) =
  let aux prec l = List.fold_left (fun pr a -> set_color cg pr a col; a) prec l in
  match c with
    | [] -> failwith "color_path: error 1"
    | [_] -> failwith "color_path: error 2"
    | a::b::t -> let _ = aux a (b::t) in ();;

  
(* Colors a cycle (a1,a2,...,an), where a1=beg, with color col *)
let color_cycle (cg : colored_graph) (c : int list) (col : color) =
  (*let rec aux prec = function
    | [] -> failwith "color_cycle: error 3"
    | [a] -> (set_color cg prec a col; a)
    | a::t -> (set_color cg prec a col; aux a t)
  in
  *)
  let aux prec l = List.fold_left (fun pr a -> set_color cg pr a col; a) prec l in
  match c with
    | [] -> failwith "color_cycle: error 1"
    | [_] -> failwith "color_cycle: error 2"
    | a::b::t -> let last = aux a (b::t) in set_color cg a last col;;

(* Color the path p with color c1 from the beginning to vertex i *)
let color_until (cg : colored_graph) (i : int) (p : int list) (c1 : color) =
  let rec aux prec = function
    | [] -> ()
    | h::t -> (set_color cg prec h c1; if h<>i then aux h t)
  in
  match p with
    | [] -> ()
    | h::t -> if h<>i then aux h t;;

(* Color the section of the path p between vertices i1 and i2 with color c1 *)
let color_section (cg : colored_graph) (i1 : int) (i2 : int) (p : int list) (c1 : color) =
  let rec aux = function
    | [] -> ()
    | h::t -> if h=i1
            then color_until cg i2 (i1::t) c1
            else aux t
  in
  aux p;;

(* Color the path p with color c1 from vertex i to the end *)
let color_from (cg : colored_graph) (i1 : int) (p : int list) (c1 : color) =
  color_section cg i1 (-1) p c1;;

(* Colors a path (a1,a2,...,an), where a1=beg, with color c1 until i1, then with color c2 *)
let rec color_path_two_colors (cg : colored_graph) (i1 : int) (c1 : color) (c2 : int) = function
    | ph::ps::pt ->
      if ps=i1
        then (set_color cg ph ps c1; color_path cg (ps::pt) c2)
        else (set_color cg ph ps c1; color_path_two_colors cg i1 c1 c2 (ps::pt))
    | _ -> ();;
(* Alternative: less efficient
let rec color_path_two_colors cg i1 c1 c2 =
  color_until cg i l c1;
  color_from cg i l c2;;
*)
