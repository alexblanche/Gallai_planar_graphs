(* Configuration C0 *)

let conf_C0 (g : graph) (u : int) =
	(degree g u = 2) &&
	(List.length g.e.(u) = 2) &&
	(let [v1;v2] = neighbors g u in
	not (is_present_edge g v1 v2));;

(* conf_C0 g2 5;; *)

(* Returns a vertex that constitutes a C0 configuration if there is one, returns None otherwise *)
let is_there_C0 (g : graph) =
	let n = number_of_vertices g in
	let rec aux i =
		if (i = n)
			then None
			else if conf_C0 g i
				then Some i
				else aux (i+1)
	in
	aux 0;;

(*
is_there_C0 g2;;
remove_edge g2 5 6;;
g2;;
is_there_C0 g2;;
add_edge g2 5 6 None;;
is_there_C0 g2;;
*)

let rule_C0 (g : graph) (u : int) =
	let [v1;v2] = neighbors g u in
	{op = [REMOVE(u,v1); REMOVE(u,v2); ADD(v1,v2); DELETE(u)];
	 reco = [DEVIATE((v1,v2),u)]};;

(*
graph_to_gen g2;;
let ru = rule_C0 g2 5 in
List.iter (reduce g2) ru.op;
graph_to_gen g2;;
(* OK *)
*)


let rec all_C0 (g : graph) (r : reduction) =
  match is_there_C0 g with
    | None -> ()
    | Some u -> (let ru = rule_C0 g u in
                reducel g ru.op;
                add_rule r ru;
                all_C0 ()
                );;