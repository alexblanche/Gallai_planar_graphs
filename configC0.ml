(* Configuration C0 *)

let conf_C0 (g : graph) (u : int) =
	(degree g u = 2) &&
	(* (List.length g.e.(u) = 2) && *)
	(match neighbors g u with
		| [v1; v2] -> not (is_present_edge g v1 v2)
		| _ -> failwith "config_C0: the degree of u does not match its number of neighbors"
	);;

(* Returns a vertex that constitutes a C0 configuration if there is one, returns None otherwise *)
let is_there_C0 (g : graph) =
	let n = number_of_vertices g in
	try
		Some (List.find (conf_C0 g) (range n))
	with
		| Not_found -> None;;

(*
is_there_C0 g2;;
remove_edge g2 5 6;;
g2;;
is_there_C0 g2;;
add_edge g2 5 6 None;;
is_there_C0 g2;;
*)

let rule_C0 (g : graph) (u : int) =
	match neighbors g u with
	| [v1;v2] ->
		{op = [REMOVE(u,v1); REMOVE(u,v2); ADD(v1,v2); DELETE(u)];
		reco = [DEVIATE((v1,v2),u)]}
	| _ -> failwith "rule_C0: u does not belong to a C0 configuration";;

(*
graph_to_gen g2;;
let ru = rule_C0 g2 5 in
List.iter (reduce g2) ru.op;
graph_to_gen g2;;
*)


let rec all_C0 (g : graph) (r : reduction) =
  match is_there_C0 g with
    | None -> ()
    | Some u ->
			(let ru = rule_C0 g u in
      reducel g ru.op;
      add_rule r ru;
      all_C0 g r);;