(* For private testing *)

(* The display is heavily bugged: if I don't synchronize after each function, half of them are ignored *)
  let print_colors () =
    let side = 30 in
    let ncol = 8 in
    init();
    let col_tab = [|black; white; red; green; blue; yellow; cyan; magenta|] in
    for i=0 to ncol-1 do
      set_color col_tab.(i);
      fill_rect (side*(i+1)) (size_y() - 70) side side;
      (* synchronize (); *)
      for j=i to ncol-1 do
        let (ri,gi,bi) = int_to_rgb col_tab.(i) in
        let (rj,gj,bj) = int_to_rgb col_tab.(j) in
        let (r,g,b) = ((ri+rj)/2, (gi+gj)/2, (bi+bj)/2) in
        set_color (rgb r g b);
        fill_rect (side*(j+1)) (size_y() - 70 - (i+1)*side) side side;
        (* synchronize (); *)
      done;
    done;
    let actual_col_tab = [|(0,0,0);	(127,127,127); (127,0,0);	(0,127,0); (0,0,127); (127,127,0); (0,127,127); (127,0,127);
      (255,255,255); (255,127,127);	(127,255,127); (127,127,255);	(255,255,127); (127,255,255);	(255,127,255);
      (255,0,0); (127,127,0);	(127,0,127); (255,127,0);	(127,127,127); (255,0,127);
      (0,255,0); (0,127,127); (127,255,0); (0,255,127);	(127,127,127);
      (0,0,255); (127,127,127);	(0,127,255); (127,0,255);
      (255,255,0); (127,255,127);	(255,127,127);
      (0,255,255); (127,127,255);
      (255,0,255)|] in
    let ancol = Array.length actual_col_tab in
    for i=0 to ancol-1 do
      let (ri,gi,bi) = actual_col_tab.(i) in
      let ci = rgb ri gi bi in
      set_color ci;
      fill_rect (side*(i+1)) 200 side side;
      moveto (side*(i+1)+(side/4)) 180;
      draw_string (string_of_int i);
      (* synchronize (); *)
    done;
    let indices_kept = [|15;26;21;30;29;1;0;35;11;10;2;3;18;4;33;5;28;7;9;6;14;20|] in
    let colors_kept = Array.map (fun i -> actual_col_tab.(i)) indices_kept in
    let nkept = Array.length colors_kept in
    for i=0 to nkept-1 do
      let (ri,gi,bi) = colors_kept.(i) in
      let ci = rgb ri gi bi in
      (* print_rgb ri gi bi; *)
      set_color ci;
      fill_rect (side*(i+1)) 100 side side;
      (* synchronize (); *)
    done;
    synchronize ();
    wait_for_space ();
    close_graph ();;


(* Example of vl, el*)
let (vl0 : vertex_coordinates list) =
  [(0, (164, 484)); (1, (327, 514)); (2, (409, 521)); (3, (566, 514));
   (4, (773, 504)); (5, (880, 522)); (6, (1028, 416)); (7, (1024, 301));
   (8, (1006, 202)); (9, (944, 148)); (10, (774, 110)); (11, (582, 128));
   (12, (384, 124)); (13, (235, 160)); (14, (143, 222)); (15, (141, 311));
   (16, (175, 366)); (17, (265, 418)); (18, (381, 427)); (19, (491, 440));
   (20, (638, 440)); (21, (772, 426)); (22, (902, 406)); (23, (869, 335));
   (24, (791, 260)); (25, (682, 224)); (26, (492, 234)); (27, (392, 253));
   (28, (322, 294)); (29, (406, 333)); (30, (506, 337)); (31, (601, 338))] in

let (el0 : simple_edge list) =
  [(24, 9); (23, 9); (24, 10); (25, 10); (10, 11); (9, 10); (9, 8); (8, 7);
   (23, 8); (23, 31); (31, 24); (31, 25); (26, 31); (11, 31); (11, 26);
   (25, 11); (24, 25); (24, 23); (21, 23); (4, 5); (20, 4); (4, 21);
   (22, 4); (5, 22); (6, 5); (7, 6); (22, 7); (21, 22); (20, 21); (19, 20);
   (19, 30); (29, 19); (29, 18); (26, 29); (27, 26); (27, 12); (13, 27);
   (28, 13); (28, 16); (28, 15); (27, 28); (29, 27); (28, 29); (17, 28);
   (1, 17); (0, 1); (18, 0); (1, 18); (2, 1); (18, 2); (19, 18); (3, 19);
   (20, 3); (31, 20); (30, 31); (26, 30); (12, 26); (13, 12); (14, 13);
   (15, 14); (16, 15); (17, 16); (0, 17)] in

let n0 = List.length vl0 in
let cg = graph_to_colored_graph (list_of_edges_to_graph n0 el0) in
List.iter (fun (i,j) -> set_edge_color cg i j (Random.int 22)) el0;
print_graph cg.cg vl0;
print_colored_graph cg vl0;;

