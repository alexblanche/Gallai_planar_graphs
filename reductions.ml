(* Types implementing the reductions *)

type color = int;;

(* Operations forming the reduced graph / rebuilding the graph *)
type operation = ADD of int * int | REMOVE of int * int | DELETE of int;;

(* Recoloring operations *)
(* NEW_COLOR(l): give a new color to the edges of l *)
(* RECOLOR((i,j),l): generic operation; give to the edges of l the color of edge ij *)
(* DEVIATE((i,j),u): deviate the color of edge ij on edges ui and uj *)
(* EXTEND(i,l): extends a color that ends on i to the edges of l *)
type recoloring = NEW_COLOR of (int * int) list
						| RECOLOR of (int * int) * (int * int) list
						| DEVIATE of (int * int) * int
						| EXTEND of (int * ((int * int) list));;

type rule = {op : operation list; reco : recoloring list};;

(* Type of a coloring *)
type coloring = (color option) array array;;

let empty_coloring (n : int) = (Array.make_matrix n n None : coloring);;

let color_edge (colo : coloring) (i : int) (j : int) (c : color) =
	colo.(i).(j) <- Some c;
	colo.(j).(i) <- Some c;;

(*
(* Type encapsulating a reduction *)
(* Reduced graph is obtained by applying the operations of red_stack in order *)
(* Recolored graph is obtained by undoing the operations of red_stack and applying the recoloring operations of col_stack *)
type reduction = {g : graph ; mutable red_stack : operation list ; col_stack : recoloring list};;
*)
type reduction = {g : graph ; mutable rules : rule list};;

let empty_reduction (g : graph) = {g = g; rules = []};;

let add_rule (r : reduction) (ru : rule) =
	let rl = r.rules in
	r.rules <- ru::rl;;

(* A graph and its coloring *)
(* nc = number of colors used *)
type colored_graph = {cg : graph; cc : coloring; mutable nc : int};;

let graph_to_colored_graph (g : graph) =
	{cg = g; cc = empty_coloring (number_of_vertices g); nc = 0};;

let get_color (cg : colored_graph) (i : int) (j : int) =
	cg.cc.(i).(j);;

let set_color (cg : colored_graph) (i : int) (j : int) (c : color) =
	cg.cc.(i).(j) <- Some c;
	cg.cc.(j).(i) <- Some c;;

let new_color (cg : colored_graph) =
	let n = cg.nc in
	cg.nc <- n+1;
	n;;


(* Applying the reduction *)

let reduce (g : graph) (op : operation) =
	match op with
		| ADD(i,j) -> add_edge g i j
		| REMOVE(i,j) -> remove_edge g i j
		| DELETE(u) -> remove_vert g u;;

let reducel (g : graph) (opl : operation list) =
	List.iter (reduce g) opl;;

let undo_red (g : graph) (op : operation) =
	match op with
		| ADD(i,j) -> remove_edge g i j
		| REMOVE(i,j) -> add_edge g i j
		| DELETE(u) -> add_vert g u;;

let undo_reds (g : graph) (opl : operation list) =
	List.iter (undo_red g) opl;;

(* Applying the recoloring *)

let rec find_unique = function
		| [] -> failwith "find_unique : no element found"
		| h::t -> if not (List.mem h t)
						then h
						else find_unique (List.filter (fun x -> x<>h) t);;

(* find_unique [2;5;1;8;3;5;8;2;7;3;1;5];; *)

let recolor (cg : colored_graph) (reco : recoloring) =
	match reco with
		| NEW_COLOR(l) -> let c = new_color cg in
								 List.iter (fun (i,j) -> set_color cg i j c) l
		| RECOLOR((a,b),l) -> let Some c = get_color cg a b in
									  List.iter (fun (i,j) -> set_color cg i j c) l
		| DEVIATE((i,j),u) -> let Some c = get_color cg i j in
									  (set_color cg u i c; set_color cg u j c)
		| EXTEND(k,l) -> let neigh = neighbors cg.cg k in
								let cl = List.map (fun i -> let Some c = get_color cg k i
																	  in c)
														 neigh in
								let c = find_unique cl in
								List.iter (fun (i,j) -> set_color cg i j c) l;;

let recolorl (cg : colored_graph) (recol : recoloring list) =
	List.iter (recolor cg) recol;;

(* General reduction function *)
let apply_reduction (rg : reduction) =
	let g' = copy_of_graph rg.g in
	rg.rules <- List.rev rg.rules; (* so that the first rule added to the rule list is the first to be applied *)
	let opll = List.map (fun x -> x.op) rg.rules in
	List.iter (reducel g') opll;
	g';;

(* General recoloring function *)
(* g' = reduced graph, obtained from apply_reduction *)
let apply_recoloring (r : reduction) (g' : colored_graph) =
	let rulesl = List.rev r.rules in
	(*
	let rec aux = function
		| [] -> g'
		| ru::t -> (undo_reds g'.cg (List.rev ru.op);
						 recolorl g' (List.rev ru.reco);
						 aux t)
	in
	aux rulesl;;*)
	let f ru = (undo_reds g'.cg (List.rev ru.op);
				   recolorl g' (List.rev ru.reco)) in
	List.iter f rulesl;
	g';;

(* Build a generic coloring *)
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