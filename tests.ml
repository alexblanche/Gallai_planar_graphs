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