(* Types (coordinates, generic graphs, colored graphs) and useful functions *)


(** Vertex and edge types for the graphical interface **)

(* (n,(x,y)), n is the index, (x,y) are the coordinates *)
type vertex_coordinates = int * (int * int);;
(* (i,j), pair of indices *)
type simple_edge = int * int;;



(** Generic graphs: simplified type **)

type gen_vertex = int;;

(* gen_graph:
	gen_n = number of vertices (0,...,n-1)
	gen_e = array t; for all i, t.(i) = neighbors of vertex i
*)
type gen_graph = {
	gen_n : int;
	gen_e : gen_vertex list array;
};;

(* Ex *)
(* let gen_g = {gen_n = 3; gen_e = [|[1;2];[0];[0]|]};; *)



(** Graphs: actual type used by the main program **)

(* vertex:
	n = index of the vertex
	vert_present = true iff the vertex is present
	d = degree of the vertex
*)
type vertex = {
	n : int;
	mutable vert_present : bool;
	mutable d : int;
};;

(* edge:
	edge_end = index of one end of the edge (in the adjacency list, the other end is implicit)
	edge_present = edge is present iff (edge_present = true and both ends are present)
*)
type edge = {
	edge_end : int;
	mutable edge_present : bool;
};;

(* graph:
	represented as an adjacency list structure
	v = array of vertices
	e = array of lists of neighbors
*)
type graph = {
	v : vertex array;
	e : edge list array;
};;


(* Accessors and mutators *)

let number_of_vertices (g : graph) = Array.length g.v;;

let degree (g : graph) (i : int) : int = g.v.(i).d;;

(* Returns the ends of the present edges in el *)
let edge_list_to_neighbors (el : edge list) : int list =
    List.map (fun e -> e.edge_end)	(List.filter (fun e -> e.edge_present) el);;

(* Returns the indices of the neighbors of the vertex of index i *)
let neighbors (g : graph) (i : int) : int list = edge_list_to_neighbors g.e.(i);;

let is_present_vert (g : graph) (i : int) : bool = g.v.(i).vert_present;;

let flip_vert (g : graph) (i : int) : unit =
	if (i < 0) || (i > number_of_vertices g)
		then failwith "flip_vert : incorrect number of vertices specified"
		else g.v.(i).vert_present <- not g.v.(i).vert_present;;

let remove_vert (g : graph) (i : int) : unit =
	if is_present_vert g i then flip_vert g i;;

let add_vert (g : graph) (i : int) : unit =
	if (i < 0) || (i > number_of_vertices g)
		then failwith "flip_vert : incorrect number of vertices specified"
		else if not (is_present_vert g i) then flip_vert g i;;

let is_present_edge (g : graph) (i : int) (j : int) : bool =
	(i >= 0 && i < number_of_vertices g) &&
	(j >= 0 && j < number_of_vertices g) &&
	(is_present_vert g i) &&
	(is_present_vert g j) &&
	(try (List.find (fun e -> e.edge_end = j) g.e.(i)).edge_present with
		| Not_found -> false;
	);;
(* is_present_edge g 0 2;; *)

(* Returns the neighbors of i (linked with i with a present edge) *)
let present_edges (g : graph) (i : int) : edge list =
	List.filter (fun x -> is_present_edge g i x.edge_end) g.e.(i);;

(* let v = {n = 1; vert_present = true};;
let e = {edge_end = 1; edge_c = None; edge_present = true};;
let l = [e] in e.edge_present <- false; l;; (* ça marche *) *)

let flip_edge (g : graph) (i : int) (j : int) : unit =
	(try
		let ej = List.find (fun e -> e.edge_end = j) g.e.(i) in
		ej.edge_present <- not ej.edge_present
	with
		| Not_found -> ();
	);
	(try
		let ei = List.find (fun e -> e.edge_end = i) g.e.(j) in
		ei.edge_present <- not ei.edge_present
	with
		| Not_found -> ();
	)
;;

let remove_edge (g : graph) (i : int) (j : int) : unit =
	if is_present_edge g i j
		then flip_edge g i j;
				g.v.(i).d <- g.v.(i).d - 1;
				g.v.(j).d <- g.v.(j).d - 1;;

let add_edge (g : graph) (i : int) (j : int) : unit =
	if (i < 0 || i >= number_of_vertices g) ||
		(j < 0 || j >= number_of_vertices g) ||
		not (is_present_vert g i) ||
		not (is_present_vert g j)
	then failwith "add_edge: absent vertices or invalid indices"
	else
		(try
			(* Case 1: the edge exists in the structure but is not present (edge_present = false) *)
			let ej = List.find (fun e -> e.edge_end = j) g.e.(i) in
			ej.edge_present <- true;
			g.v.(i).d <- g.v.(i).d + 1;
			try
				let ei = List.find (fun e -> e.edge_end = i) g.e.(j) in
				ei.edge_present <- true;
				g.v.(j).d <- g.v.(j).d + 1;
			with
				| Not_found -> failwith "add_edge: edge present in i but not in j"
		 with
			| Not_found ->
				 (g.e.(i) <- {edge_end = j; edge_present = true}::g.e.(i);
				  g.e.(j) <- {edge_end = i; edge_present = true}::g.e.(j);
				  g.v.(i).d <- g.v.(i).d + 1;
				  g.v.(j).d <- g.v.(j).d + 1)
		)
;;
(*
g;;
add_edge g 1 2;;
graph_to_gen g;;
remove_edge g 1 2;;
graph_to_gen g;;
remove_edge g 1 0;;
graph_to_gen g;;
add_edge g 1 2;;
graph_to_gen g;;
(* color_of_edge g 1 2;; *)
g;;
*)




(** Conversions **)

let list_of_edges_to_gen (n : int) (el : simple_edge list) : gen_graph =
	let ge = Array.make n [] in
	List.iter (fun (i,j) -> (ge.(i) <- j::ge.(i); ge.(j) <- i::ge.(j))) el;
	{gen_n = n; gen_e = ge};;
(* let gen_2 = list_of_edges_to_gen 3 [(0,1);(1,2)];; *)

let gen_to_vertex (gv : gen_vertex) : vertex =
	{n = gv; vert_present = true; d = 0};;

let gen_to_graph (h : gen_graph) : graph =
	let t_vert = Array.init h.gen_n
		(fun gv -> let v = gen_to_vertex gv in
		(v.d <- List.length h.gen_e.(gv);v))
	in
	let t_edge = Array.init h.gen_n
		(fun i ->
			List.map (fun j -> {edge_end = j; edge_present = true}) h.gen_e.(i))
	in
	{v = t_vert; e = t_edge};;
(* let g = gen_to_graph gen_g;; *)

let graph_to_gen (g : graph) : gen_graph =
	{gen_n = number_of_vertices g;
	gen_e = Array.map	edge_list_to_neighbors g.e};;
(* let gen_g2 = graph_to_gen g;; *)

(* Conversion of a list of simple_edge to a graph *)
(* n is the number of edges *)
let list_of_edges_to_graph (n : int) (el : simple_edge list) : graph =
	let gen_g = list_of_edges_to_gen n el in
	gen_to_graph gen_g;;

(* Conversion of a sequence of vertices to a list of edges (of type simple_edge list) *)
(* seq_to_list_of_edges [2;3;5] 0 = [(2,0);(3,2);(5,3)] *)
let seq_to_list_of_edges (seq : int list) (vdep : int) : simple_edge list =
	let (acc,_) = List.fold_left (fun (acc,v) x -> ((x,v)::acc, x)) ([],vdep) seq in
	acc;;
(* seq_to_list_of_edges [2;3;5] 0;; *)

(* Returns the list of edges (simple_edge list) of the graph *)
let graph_to_list_of_edges (g : graph) : simple_edge list =
	let n = number_of_vertices g in
	let add_incident_edges acc0 i =
		let neigh = neighbors g i in
		(* if x>i: in order to avoid duplicates *)
		List.fold_left (fun acc x -> if x>i then (i,x)::acc else acc) acc0 neigh
	in
	List.fold_left add_incident_edges [] (range n);;
(* Complexity = O(|G|), because neighbors = O(degree),
	 so graph_to_list_of_edges = O(sum of the degrees) = O(number of edges) *)




(** Miscellaneous graph-related functions **)

(* Returns the number of edges of the graph induced by a list of vertices *)
let count_edges (g : graph) (vl : int list) : int =
	count (fun (v,v') -> is_present_edge g v v') (pairs_of vl);;

(* Returns a copy of the graph g *)
let copy_of_graph (g : graph) : graph =
	let v' = Array.copy g.v in
	let e' = Array.copy g.e in
	{v = v'; e = e'};;

(* Generates a basic embedding of a graph, in a window 1200*600 *)
let generate_embedding (n : int) =
	let x = 1200. in
	let y = 600. in
	let pi = 3.14159265 in
	let theta = 2.*.pi/.(float_of_int n) in
	let r = 0.33 *. y in
	List.map (fun i -> (i,(int_of_float ((x/.2.)+.r*.cos ((float_of_int i)*.theta)),int_of_float ((y/.2.)+.r*.sin ((float_of_int i)*.theta))))) (range n);;