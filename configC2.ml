(* Configurations CII *)

#use "K4subd.ml";;

(*
CII
	- Implement a function that returns a K4-subdivision in an almost 4-connected planar graph (Yu? KKB?)
  - Define all patterns: CT1 of int list | ...
  - Define a rule for each of the configurations D,J,R
  - is_there_C2: returns four vertices and the type of CII configuration (implemented like the tree of cases)
  - combine_patterns pl s: returns a rule that combines a set of 2 to 4 patterns (from a list pl), associated with the subdivision (s: edge list)
*)

let rec all_C2 (g : graph) (r : reduction) =
    ()
;;