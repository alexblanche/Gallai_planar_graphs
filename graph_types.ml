(* Types (generic graphs, colored graphs) and useful functions *)



(* Generic graphs *)

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

(* Generation *)

let list_of_edges_to_gen (n : int) (el : (int * int) list) =
	let ge = Array.make n [] in
	let rec aux = function
		| [] -> {gen_n = n; gen_e = ge}
		| (i,j)::t -> (ge.(i) <- j::ge.(i);
							 ge.(j) <- i::ge.(j);
							 aux t)
	in
	aux el;;

(* let gen_2 = list_of_edges_to_gen 3 [(0,1);(1,2)];; *)

(* vertex:
	n = index
	vert_present = true iff the vertex is present
	d = degree
*)
type vertex = {
	n : int;
	mutable vert_present : bool;
	mutable d : int;
};;

(* edge:
	edge_end = one end of the edge (in the adjacency list, the other end is implicit)
	edge_c = color of the edge, if some
	edge_present = edge is present iff (edge_present = true and both ends are present)
*)
type edge = {
	edge_end : int;
	(* mutable edge_c : color option; *)
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

(* let color_of_edge (g : graph) (i : int) (j : int) =
	(List.find (fun e -> e.edge_end = j) g.e.(i)).edge_c;; *)

let degree (g : graph) (i : int) = g.v.(i).d;;

let edge_list_to_neighbors (l : edge list) =
    List.map (fun e -> e.edge_end)	(List.filter (fun e -> e.edge_present) l);;

let neighbors (g : graph) (i : int) = edge_list_to_neighbors g.e.(i);;

let is_present_vert (g : graph) = fun i -> g.v.(i).vert_present;;

let flip_vert (g : graph) (i : int) =
	if (i < 0) || (i > number_of_vertices g)
		then failwith "flip_vert : incorrect number of vertices specified"
		else g.v.(i).vert_present <- not g.v.(i).vert_present;;

let remove_vert (g : graph) (i : int) =
	if is_present_vert g i then flip_vert g i;;

let add_vert (g : graph) (i : int) =
	if (i < 0) || (i > number_of_vertices g)
		then failwith "flip_vert : incorrect number of vertices specified"
		else if not (is_present_vert g i) then flip_vert g i;;

let is_present_edge (g : graph) (i : int) (j : int) =
	(i >= 0 && i < number_of_vertices g) &&
	(j >= 0 && j < number_of_vertices g) &&
	(is_present_vert g i) &&
	(is_present_vert g j) &&
	(try (List.find (fun e -> e.edge_end = j) g.e.(i)).edge_present with
		| Not_found -> false;
	);;

(* is_present_edge g 0 2;; *)

let present_edges (g : graph) (i : int) =
	List.filter (fun x -> is_present_edge g i x.edge_end) g.e.(i);;

(* let v = {n = 1; vert_present = true};;
let e = {edge_end = 1; edge_c = None; edge_present = true};;
let l = [e] in e.edge_present <- false; l;; (* ça marche *) *)

let flip_edge (g : graph) (i : int) (j : int) =
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

let remove_edge (g : graph) (i : int) (j : int) =
	if is_present_edge g i j
		then flip_edge g i j;
				g.v.(i).d <- g.v.(i).d - 1;
				g.v.(j).d <- g.v.(j).d - 1;;

let add_edge (g : graph) (i : int) (j : int) (*(c : color option)*) =
	if (i < 0 || i >= number_of_vertices g) ||
		(j < 0 || j >= number_of_vertices g) ||
		not (is_present_vert g i) ||
		not (is_present_vert g j)
	then failwith "add_edge : absent vertices or invalid indices"
	else
		(try
			let ej = List.find (fun e -> e.edge_end = j) g.e.(i) in
			(* ej.edge_c <- c; *)
			ej.edge_present <- true;
			g.v.(i).d <- g.v.(i).d + 1;
			try
				let ei = List.find (fun e -> e.edge_end = i) g.e.(j) in
				(* ei.edge_c <- c; *)
				ei.edge_present <- true;
				g.v.(j).d <- g.v.(j).d + 1;
			with
				| Not_found -> failwith "add_edge : edge present in i but not in j"
		 with
			| Not_found ->
				 (g.e.(i) <- {edge_end = j; (* edge_c = c; *) edge_present = true}::g.e.(i);
				  g.e.(j) <- {edge_end = i; (* edge_c = c; *) edge_present = true}::g.e.(j);
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




(* Conversions *)

let gen_to_vertex (gv : gen_vertex) = {n = gv; vert_present = true; d = 0};;

let gen_to_graph (h : gen_graph) =
	let t_vert = Array.init h.gen_n
						(fun gv -> let v = gen_to_vertex gv in
									   (v.d <- List.length h.gen_e.(gv);v))
	in
	let t_edge = Array.init h.gen_n
		(fun i -> List.map
						(fun j ->
							{edge_end = j;
							 (* edge_c = None; *)
							 edge_present = true}
						)
						h.gen_e.(i)
		) in
	{
		v = t_vert;
		e = t_edge;
	};;

(* let g = gen_to_graph gen_g;; *)

let graph_to_gen (g : graph) = {
	gen_n = number_of_vertices g;
	gen_e = Array.map
				edge_list_to_neighbors
				g.e;
};;

(* let gen_g2 = graph_to_gen g;; *)

(* Conversion of a sequence of vertices to a list of edges *)
(* seq_to_list_of_edges [2;3;5] 0 = [(2,0);(3,2);(5,3)] *)

let seq_to_list_of_edges (seq : int list) (vdep : int) =
	let rec aux l v = function
		| [] -> l
		| x::t -> aux ((x,v)::l) x t
	in
	aux [] vdep seq;;

(* seq_to_list_of_edges [2;3;5] 0;; *)


(* Miscellaneous graph-related functions *)

(* Returns the number of edges of the graph induced by a list of vertices *)
let count_edges (g : graph) (vl : int list) =
	count (fun (v,v') -> is_present_edge g v v') (pairs_of vl);;

let copy_of_graph (g : graph) =
	let v' = Array.copy g.v in
	let e' = Array.copy g.e in
	{v = v'; e = e'};;

