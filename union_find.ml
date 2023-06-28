(* Implementation of the Union-Find data structure *)

type uf_elt = {mutable parent : int; mutable rank : int};;
type union_find = uf_elt array;;

let get_parent (uf : union_find) (i : int) =
  uf.(i).parent;;

let set_parent (uf : union_find) (i : int) (j : int) =
  uf.(i).parent = j;;

let get_rank (uf : union_find) (i : int) =
  uf.(i).rank;;

let set_rank (uf : union_find) (i : int) (r : int) =
  uf.(i).rank = r;;

(* Returns a fresh, initialized union-find structure with elements 0,...,n-1 *)
let init_union_find n =
  Array.init n (fun i -> {parent = i; rank = 0});;

let rec find (uf : union_find) (i : int) =
  let ipar = get_parent uf i in
  if ipar <> i
    then
      let iroot = find uf ipar in
      set_parent uf i iroot;
  get_parent uf i;;

let union (uf : union_find) (i : int) (j : int) =
  let iroot = find uf i in
  let jroot = find uf j in
  if iroot <> jroot
    then if get_rank uf iroot < get_rank uf jroot
      then set_parent uf iroot jroot
      else
        (set_parent uf jroot iroot;
        let irootrank = get_rank uf iroot in
        if irootrank = get_rank uf jroot
          then set_rank uf iroot (irootrank + 1));;


(* Useful functions *)

(* Returns true if all the elements from l belong to the same component in uf *)
let check_one_component (uf : union_find) (l : int list) =
  try
    let i = List.hd l in
    let root = get_parent uf i in
    List.for_all (fun i -> (find uf i) = root) l
  with
    | Invalid_argument -> failwith "check_one_component : index out of bounds"
    | _ -> failwith "check_one_component : empty structure"
  ;;