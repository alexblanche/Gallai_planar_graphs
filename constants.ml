(* Constants *)

(* 22 sufficiently different colors *)
let color_list = [|
(255,0,0);
(0,0,255);
(0,255,0);
(255,255,0);
(127,0,255);
(127,127,127);
(0,0,0);
(255,0,255);
(127,127,255);
(127,255,127);
(127,0,0);
(0,127,0);
(255,127,0);
(0,0,127);
(0,255,255);
(127,127,0);
(0,127,255);
(127,0,127);
(255,127,127);
(0,127,127);
(255,127,255);
(255,0,127)|]
;;

(* Radius of the vertices in the graphical interface *)
let vertex_radius = 4;;

(* Width of the colored edges in the graphical interface (uncolored have width 1) *)
let edge_width = 3;
