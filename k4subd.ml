(* Computation of a K4-subdivision *)

(* Yu's algorithm seems incredibly difficult to implement *)
(* I suggest looking for a more efficient algorithm... (Kawarabayashi, Kobayashi, Reed algo in O(n^2)?) *)

(* Start with a heuristic algorithm? Like doing 6 dijkstras and see if they intersect *)

(* Returns an array t of 6 paths (int lists) and a boolean b:
   In case of a K4-subdivision: b = true and t = u1~u2, u1~u3, u1~u4, u2~u3, u2~u4, u3~u4 in this order
   In case of a C4+-subdivision: b = false and t = u1~u2, the two paths u1~u3, the two paths u2~u4, u3~u4 in this order
*)
(* TODO *)
let compute_subdivision (g : graph) (u1 : int) (u2 : int) (u3 : int) (u4 : int) =
  ([|[];[];[];[];[];[]|], true);;