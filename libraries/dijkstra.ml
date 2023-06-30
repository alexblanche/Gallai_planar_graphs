(* Dijkstra's algorithm *)

let dijkstra_find_min (q : int list) (d : int array) (vdep : int) =
	let rec aux l vert = function
		| [] -> vert,l
		| v::t -> if d.(v) <> (-1) && (d.(vert) = (-1) || d.(v) < d.(vert))
			then aux (vert::l) v t
			else aux (v::l) vert t
	in
	aux [] vdep q;;

let dijkstra_update (d : int array) (prev : int array) (v1 : int) (v2 : int) =
	if d.(v1)<>(-1) && (d.(v2)=(-1) || d.(v2) > d.(v1) + 1)
		then (d.(v2) <- d.(v1) + 1;
			prev.(v2) <- v1);;

let dijkstra_prev (g : graph) (vdep : int) =
	let n = number_of_vertices g in
	let d = Array.make n (-1) in
	d.(vdep) <- 0;
	let prev = Array.make n (-1) in
	let q = range n in
	let rec aux = function
		| [] -> prev
		| qh::qt -> let v,l = dijkstra_find_min qt d qh in
			let neighbors_v = edge_list_to_neighbors g.e.(v) in
			(List.iter (dijkstra_update d prev v) neighbors_v;
			aux l)
	in
	aux q;;

let dijkstra_find_seq (prev : int array) (vdep : int) (varr : int) =
	let rec aux l x =
		if x <> vdep
			then aux (x::l) prev.(x)
			else l
	in
	aux [] varr;;

(* Shortest path in g, from v1 to v2 *)
let shortest_path (g : graph) (v1 : int) (v2 : int) =
	dijkstra_find_seq (dijkstra_prev g v1) v1 v2;;

(*
g;;
graph_to_gen g;;
let prev = dijkstra_prev g 0 in dijkstra_find_seq prev 0 1;;

let gen2 = list_of_edges_to_gen 7 [(2,6);(0,1);(0,2);(1,2);(2,3);(3,4);(3,5);(5,6)];;
let g2 = gen_to_graph gen2;;
shortest_path g2 0 5;;
*)