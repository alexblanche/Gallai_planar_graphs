(* Miscellaneous non-graph-related functions *)

(* Counts the number of elements of l that satisfy the predicate p *)
let count p l = List.length (List.filter p l);;

(* Returns the list [0,...,n-1] *)
let range (n : int) =
	let rec aux acc k =
		if k=0
			then 0::acc
			else aux (k::acc) (k-1)
	in
	aux [] (n-1);;

(* Generates all the pairs of elements of l *)
let pairs_of l =
	let pair_x x = List.map (fun y -> (x,y)) in
	let rec aux l acc =
		match l with
			| [] -> acc
			| h::t -> aux t (List.rev_append (pair_x h t) acc)
	in
	aux l [];;

(* Returns the position of an element of array t that satisfies the predicate f *)
(* If no such element exists, the function raises the exception Not_found (like List.find) *)
let array_find (f : 'a -> bool) (t : 'a array) =
	let n = Array.length t in
	let rec aux i =
		if i = n
			then raise Not_found
			else if f t.(i)
				then i
				else aux (i+1)
	in
	aux 0;;

(* Returns the last element of a list *)
let rec find_last = function
  | [] -> failwith "find_last: empty list"
  | [a] -> a
  | _::t -> find_last t;;
(* For fun:
let find_last = function
  | [] -> failwith "find_last: empty list"
  | a::t -> List.fold_left (fun _ x -> x) a t;;
*)

(* Returns an element present exactly one in a list *)
(* If no such element exists, the function raises the exception Not_found (like List.find) *)
let rec find_unique = function
		| [] -> raise Not_found
		| h::t -> if not (List.mem h t)
						then h
						else find_unique (List.filter (fun x -> x<>h) t);;

(* find_unique [2;5;1;8;3;5;8;2;7;3;1;5];; *)

(* Returns the first intersection of the lists l1 and l2, and the end of the list l1 after the intersection
If the lists are disjoint, raises Not_found *)
let rec find_first_inter (l1 : 'a list) (l2 : 'a list) =
  match l1 with
    | [] -> raise Not_found
    | h::t ->
      if List.mem h l2
        then (h,t)
        else find_first_inter t l2;;

(* Returns a maximum element of the list l *)
let max_list (l : int list) =
	match l with
	| [] -> failwith "max: empty list"
	| h::t ->	List.fold_left (fun a x -> if x>a then x else a) h t

(* Returns an element of the list l that minimizes function f *)
let min_list (l : 'a list) (f : 'a -> float) =
	match l with
		| [] -> raise Not_found
		| h::t -> List.fold_left (fun a x -> if f x < f a then x else a) h t;;