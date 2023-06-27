(* Coloring components: safety algorithm *)
(* The goal is to decompose a component, made up of a cycle and a path with an intersection of at most 5 vertices, into two paths *)
  
let is_component_k3 (g : graph) (vl : int list) =
  match vl with
    | [v1;v2;v3] -> (is_present_edge g v1 v2) && (is_present_edge g v1 v3) && (is_present_edge g v2 v3)
    | _ -> false;;
 
let color_k3 (cg : colored_graph) (vl : int list) =
  let c = new_color cg in
  match vl with
    | [v1;v2;v3] -> (set_color cg v1 v2 c; set_color cg v1 v3 c; set_color cg v2 v3 c)
    | _ -> failwith "color_k3: component is not a K3";;
  
let is_component_k5m (g : graph) (vl : int list) =
  List.length vl = 5 && count_edges g vl = 9;;
  
let find_the_non_edge_k5m (g : graph) (vl : int list) =
  match vl with
    | [v1;v2;v3;v4;v5] ->
      List.find (fun (v,v') -> not (is_present_edge g v v'))  (pairs_of vl)
    | _ -> failwith "find_the_non_edge: component is not a K5-";;
 
(* k5m = a,b,c,d,e; coloring = path(a,c,e,d,b), cycle(a,d,c,b,e,a) *)
let color_k5m (cg : colored_graph) (vl : int list) =
  let (a,b) = find_the_non_edge_k5m cg.cg vl in
  let rest = List.filter (fun x -> x<>a && x<>b) vl in
  match rest with
    | [c;d;e] -> let pa = new_color cg in
              (set_color cg a c pa;
               set_color cg c e pa;
               set_color cg e d pa;
               set_color cg d b pa;
               let co = new_color cg in
               set_color cg a d co;
               set_color cg d c co;
               set_color cg c b co;
               set_color cg b e co;
               set_color cg e a co)
    | _ -> failwith "color_k5m: Component not a K5-";;

(* Returns a (naive) coloring with cycles for K3,K5- and only paths for the other components *)
let pc_coloring (cg : colored_graph) (components : int list list) =
  (*let rec aux = function
    | [] -> ()
    | comp::t -> (if is_component_k3 cg.cg comp
              then color_k3 cg comp
              else if is_component_k5m cg.cg comp
                  then color_k5m cg comp
                  else naive_coloring cg comp;
              aux t)
  in
  aux components;;*)
  let f comp = if is_component_k3 cg.cg comp
              then color_k3 cg comp
              else if is_component_k5m cg.cg comp
                  then color_k5m cg comp
                  else naive_coloring cg comp
  in List.iter f components;;

  
(* Returns a list of components, as a list of vertices *)
let split_components (g : graph) =
  let n = number_of_vertices g in
  let t = 	Array.make n false in
    let rec dfs el comp =
    match el with
      | [] -> comp
      | j::q -> if not t.(j.edge_end) then
                (t.(j.edge_end) <- true;
                 let ejl = present_edges g j.edge_end in
                 let comp' = dfs ejl comp in
                   dfs q (j.edge_end::comp'))
              else dfs q comp
  in
  let rec aux compl =
    try
      let i = array_find (fun v -> (is_present_vert g v.n) && not t.(v.n)) g.v in
      let el = present_edges g i in
      let comp = dfs el [] in
      aux (comp::compl)
    with
      | Not_found -> compl
  in
  aux [];;
  
(*
  graph_to_gen g2;;
  let g3 = copy_of_graph g2;;
  remove_edge g3 2 3;;
  remove_edge g3 2 6;;
  graph_to_gen g3;;
  let [l1;l2] = split_components g3 in
  is_component_k3 g3 l2;;
*)
  
(* Colors a cycle (a1,a2,...,an), where a1=beg, with color col *)
let color_whole_cycle (cg : colored_graph) (c : int list) (col : color) =
  (*let rec aux prec = function
    | [] -> failwith "color_whole_cycle: error 3"
    | [a] -> (set_color cg prec a col; a)
    | a::t -> (set_color cg prec a col; aux a t)
  in
  *)
  let aux prec l = List.fold_left (fun pr a -> set_color cg pr a col; a) prec l in 
  match c with
    | [] -> failwith "color_whole_cycle: error 1"
    | [_] -> failwith "color_whole_cycle: error 2"
    | a::b::t -> let last = aux a (b::t) in set_color cg a last col;;
  
(* Colors a path (a1,a2,...,an), where a1=beg, with color c1 until i1, then with color c2 *)
let rec color_path (cg : colored_graph) (i1 : int) (c1 : color) (c2 : int) = function
    | ph::ps::pt ->
      if ps=i1
        then (set_color cg ph ps c1; color_path cg i1 c1 c2 (ps::pt))
        else (set_color cg ph ps c1; color_path cg i1 c1 c2 (ps::pt)) (* Error here?? *)
    | _ -> ();;
  
(* Returns the predecessor and successor of i1 in the path p *)
let find_succ_path (i1 : int) (p : int list) =
  let rec aux prec = function
    | [] -> failwith "find_succ_path: not found 1"
    | [a] -> if a=i1 then (Some prec,None) else failwith "find_succ_path: not found 2"
    | a::b::t -> if a=i1 then (Some prec,Some b) else aux a (b::t)
  in
  match p with
    | [] -> failwith "find_succ_path: not found 3"
    | [_] -> failwith "find_succ_path: not found 4"
    | h1::h2::t -> if h1=i1 then (None,Some h2) else aux h1 (h2::t);;
  
(* Returns the predecessor and successor of i1 in the cycle c *)
let find_succ_cycle (i1 : int) (c : int list) =
  let rec aux ch prec = function
    | [] -> failwith "find_succ_cycle: not found 1"
    | [a] -> if a=i1 then (prec,ch) else failwith "find_succ_cycle: not found 2"
    | a::b::t -> if a=i1 then (prec,b) else aux ch a (b::t)
  in
  match c with
    | [] -> failwith "find_succ_cycle: not found 3"
    | [_] -> failwith "find_succ_cycle: not found 4"
    | h1::h2::t -> if h1=i1 then (h2,find_last t) else aux h1 h1 (h2::t);;

(* Color the path p with color c1 from the beginning to vertex i *)
let color_until (cg : colored_graph) (i : int) (p : int list) (c1 : color) =
  let rec aux prec = function
    | [] -> ()
    | h::t -> (set_color cg prec h c1; if h<>i then aux h t)
  in
  match p with
    | [] -> ()
    | h::t -> if h<>i then aux h t;;

(* Color the section of the path p between vertices i1 and i2 with color c1 *)
let color_section (cg : colored_graph) (i1 : int) (i2 : int) (p : int list) (c1 : color) =
  let rec aux = function
    | [] -> ()
    | h::t -> if h=i1
            then color_until cg i2 (i1::t) c1
            else aux t
  in
  aux p;;

(* Color the path p with color c1 from vertex i to the end *)
let color_from (cg : colored_graph) (i1 : int) (p : int list) (c1 : color) =
  color_section cg i1 (-1) p c1;;
  
(*
(* colors p with c1 until i1, then c2; colors c with c2 except the edge i1j1 with c1 *)
let color_1inter (cg : colored_graph) (p : int list) (i1 : int) (c : int list) (c1 : color) (c2 : color) =
  match c with
    | [] -> failwith "color_1inter: empty cycle"
    | ch::_ -> (color_whole_cycle cg ch c c2;
             let (j1,_) = find_succ_cycle i1 c in
             set_color cg i1 j1 c1;
             color_path cg i1 c1 c2 p);;
  
let color_2inter (cg : colored_graph) (p : int list) (i1 : int) (i2 : int) (c : int list) (c1 : color) (c2 : color) =
  match c with
    | [] -> failwith "color_2inter: empty cycle"
    | ch::_ -> (color_whole_cycle cg ch c c2;
             let (j2,j2') = find_succ_cycle i2 c in
             set_color cg i2 (if List.mem j2 p then j2' else j2) c1;
             color_path cg i2 c1 c2 p);;
*)
  
(*
let color_ninter (cg : colored_graph) (p : int list) (c : int list) (i4 : int) (i5 : int)  (c1 : color) (c2 : color) =
  match c with
    | [] -> failwith "color_ninter: empty cycle"
    | ch ::_ -> (color_whole_cycle cg ch c c2;
              let (j5,j5') = find_succ_cycle i5 c in
              if not (List.mem j5 p)
                then (set_color cg i5 j5 c1; color_path cg i5 c1 c2 p)
                else if not (List.mem j5' p)
                  then (set_color cg i5 j5' c1; color_path cg i5 c1 c2 p)
                  else
*)
(* Does not work *)
  
(*
(* Chu's algorithm *)
(* Warning, I ignore the condition "C as large as possible". Does it work? *)
  
(* j1,j1' = neighbors of i1 on c *)
(* assuming one of them does not belong to p *)
let decompPC1 (cg : colored_graph) (p : int list) (c : int list) (i1 : int) (c1 : color) (c2 : color) =
  let (j1,j1') = find_succ_cycle i1 c in
  color_until cg i1 p c1;
  color_from cg i1 p c2;
  color_whole_cycle cg c c1;
  set_color cg i1 (if not (List.mem j1 p) then j1 else j1') c2;;
 
(* We assume j1,j1' belong to p *)
(* j2,j3 = predecessors of j1,j1' resp. on P *)
let decompPC2 (cg : colored_graph) (p : int list) (c : int list) (i1 : int) (c1 : color) (c2 : color) =
  let (j1,j1') = find_succ_cycle i1 c in
  let (Some j2,_) = find_succ_path j1 p in
  let (Some j3,_) = find_succ_path j1' p in
  (* Assuming j2 does not belong to c, switch j1,j1' and j2,j3 if needed *)
  let aux (j1 : int) (j1' : int) (j2 : int) (j3 : int) =
    color_until cg i1 p c1;
    color_whole_cycle cg c c1;
    set_color cg i1 j1 c2;
    set_color cg j1 j2 c1;
    color_section cg i1 j2 p c2;
    color_from cg j1 p c2
  in
  if not (List.mem j2 c) then aux j1 j1' j2 j3 else aux j1' j1 j3 j2;;

(* ... *)
  
(* Maybe it's a bad idea. The proof is too abstract, with "w.l.o.g." and the assumption that C is as large as possible, difficult to ensure. *)
   
  
  
  
  
(* Colors a path p and a cycle c with a path-decomposition of 2 colors *)
let decomp_cyclepath (cg : colored_graph) (p : int list) (c : int list) (c1 : color) (c2 : color) =
  let i1,p1 = find_first_inter p c in (* By hypothesis, c and p have at least one vertex in common *)
  let c1 = new_color cg in
  let c2 = new_color cg in
  try
    let i2,p2 = find_first_inter p1 c in
    try
      let i3,p3 = find_first_inter p2 c in
      try
        let i4,p4 = find_first_inter p3 c in
        try
          let i5,p5 = find_first_inter p4 c in
          () (* Algo |P cap C| = 5 *)
        with
          | Not_found -> ()(* Algo |P cap C| = 4 *)
      with
        | Not_found -> ()(* Algo |P cap C| = 3 *)
    with
      | Not_found -> ()(* color_2inter cg p i1 i2 c c1 c2 *) (* Algo |P cap C| = 2 *)
  with
    | Not_found -> ()(* color_1inter cg p i1 c c1 c2 *) (* Algo |P cap C| = 1 *)
;;
(* To be completed *)
*)

(**************************************************************************************************************************)

(* Maybe try to compute a decomposition of C + P into 2P from scratch with a specific algorithm? *)
(* Problem: it's an NP-complete problem. *)

