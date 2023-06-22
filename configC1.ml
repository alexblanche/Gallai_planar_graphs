(* Configurations CI *)

(*
  CI
	- Define type of half-rule: CV of int list | CN of int list | ...
	- Define type of CI configuration: A of int list | B1 of int list | ... | COMP of (half * half * edge list)
	- is_there_C1: returns a pair of vertices and the type of CI configuration (implemented like the tree of cases and not like is_there_C0)
	- Define a rule of each case of "close" CI configuration
	- Define a half-rule for each case of case of half-rule
	- combine_half h1 h2 s: returns a rule that combines 2 half-rules h1 and h2, associated with the path (s : edge list)
*)

let rec all_C1 (g : graph) (r : reduction) =
  ()
;;