(* Checking properties: path, decomposition *)

(* Colors are 0,...,n-1 *)

(** Useful functions **)

(* All the vertices that touch the color col *)
(* vl : int list *)
let vertices_col cg col vl = List.filter (fun i -> touches_color cg i col) vl;;

let is_color_col cg col i e = (get_edge_color cg i e.edge_end) = Some col;;

(* Edges incident with i that are colored col *)
let neigh_col cg col i = List.filter (is_color_col cg col i) (present_edges cg.cg i);;


(** Checks that a color col is a path **)
let is_a_path (cg : colored_graph) (col : color) =
  let n = number_of_vertices cg.cg in
  let vertex_list = List.filter (is_present_vert cg.cg) (range n) in
  let vcol = vertices_col cg col vertex_list in
  
  (* First check: all vertices touch the color col at most twice *)
  let count_touch = Array.make n 0 in
  let save_and_check i =
    let ncol = List.length (neigh_col cg col i) in
    (count_touch.(i) <- ncol; ncol >= 3)
  in
  if List.exists save_and_check vcol
    then false
    else (* Second check: there are exactly two vertices of degree 1 *)
      if (count (fun i -> count_touch.(i) = 1) vcol) <> 2
        then false
        else (* Final check: there is only one col component *)
          (let uf = init_union_find n in
          List.iter (fun i -> List.iter (fun e -> union uf i e.edge_end) (neigh_col cg col i)) vcol;
          check_one_component uf vcol)
;;
(* Complexity: O(n) *)


(** Checks that the coloring of the colored graph cg is a path-decomposition **)
let is_a_path_decomposition (cg : colored_graph) =
  let ncol = number_of_colors cg in
  List.for_all (is_a_path cg) (range ncol);;
(* Complexity: O(n^2) for a good decomposition, O(n^3) in worst case *)

(* Checks that the coloring of the colored graph cg is a floor(n/2)-path-decomposition *)
let is_a_good_decomposition (cg : colored_graph) =
  (is_a_path_decomposition cg) && (number_of_colors cg <= (number_of_vertices cg.cg)/2);;