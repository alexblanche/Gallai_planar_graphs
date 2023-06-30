(* Graphical interface for graphs *)

#load "Graphics.cma";;
open Graphics;;

(* Constants *)
let vertex_radius = 4;;

(* Euclidian distance *)
let dist (x : int) (y : int) (a : int) (b : int) =
	let (x',y',a',b') =
			(float_of_int x, float_of_int y,
			 float_of_int a, float_of_int b)
	in
	sqrt ((x'-.a')*.(x'-.a')+.(y'-.b')*.(y'-.b'));;

(* Draws a vertex with radius vertex_radius, in color col, on coordinates (x,y) *)
let draw_vertex (x : int) (y : int) (col : Graphics.color) =
	draw_circle x y vertex_radius;
	let fg = foreground in
	set_color col;
	fill_circle x y (vertex_radius-1);
	set_color fg;;

(* Draws an edge between points of coordinates (x,y) and (a,b) *)
let draw_edge (x : int) (y : int) (a : int) (b : int) =
	moveto x y;
	lineto a b;;

(* Returns the vertex closest with the point of coordinates (x,y),
	 if it is located within radius d *)
let rec closest_vertex (x : int) (y : int) (d : float) (l : vertex_coordinates list) : vertex_coordinates option =
	try
		let (k,(a,b)) = min_list l (fun (_,(a,b)) -> dist x y a b) in
		if (dist x y a b) < d
			then Some (k,(a,b))
			else None
	with
		| Not_found -> None

(* Displays the background with the function bg, then the vertices in the list vl and the edges in the list el *)
let display (vl : vertex_coordinates list) (el : simple_edge list) (bg : unit -> unit) =
	let display_edge_pair (a,b) =
		let (x,y) = List.assoc a vl in
		let (z,u) = List.assoc b vl in
		draw_edge x y z u
	in
	clear_graph ();
	moveto 0 0;
	bg ();
	List.iter display_edge_pair el;
	List.iter (fun (_,(x,y)) -> draw_vertex x y white) vl;
	synchronize ();;

(* Modifies the coordinates of the vertex of index i to (new_x,new_y) *)
let move_vertex_position (i : int) (new_x : int) (new_y : int) (vl : vertex_coordinates list) : vertex_coordinates list =
	let aux ((k,(x,y)) : vertex_coordinates) =
		if k <> i
			then (k,(x,y))
			else (k,(new_x,new_y))
	in
	List.rev_map aux vl;;

(* Initializes the window *)
let init () =
	open_graph " 1200x600";
	(* Note: resize_window does not work *)
	set_window_title "Visualizer";
	set_color black;
	display_mode false;
	remember_mode true;;

(* Interface for vertex addition *)
(* Returns the list of vertices added (not the ones already present) *)
(* np is the number of vertices already present, vlp is the list of these vertices,
	 elp is the list of edges between these vertices *)
let add_vertices (np : int) (vlp : vertex_coordinates list) (elp : simple_edge list) : vertex_coordinates list =
	let bg1 () = draw_string "Press [Space] to add edges" in
	display vlp elp bg1;
	
	let vl = ref vlp in				(* List of all the vertices *)
	let added_vl = ref [] in	(* List of added vertices *)
	let n = ref np in					(* Total number of vertices *)
	let exit = ref false in
		
	while not !exit do
		let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [Button_down; Key_pressed]
		in
		exit := key = ' ';

		if button then
			begin
				let new_vert = (!n, (mouse_x,mouse_y)) in
				vl := new_vert::!vl;
				added_vl := new_vert::!added_vl;
				n:= !n+1;
				draw_vertex mouse_x mouse_y white;
				synchronize()
			end;
	done;
	!added_vl;;

(* Interface for edge addition *)
(* Returns the list of edges added (not the ones already present) *)
(* vlp is the list of present vertices, elp is the list of edges between these vertices *)
let add_edges (vlp : vertex_coordinates list) (elp : simple_edge list) : simple_edge list =
	let bg2 () = draw_string "Click on two vertices to add an edge between them, press [a] to finish" in
	display vlp elp bg2;
	
	let el = ref elp in 			(* List of all the edges *)
	let added_el = ref [] in	(* List of added edges *)

	let exit = ref false in
	let selec = ref false in
	let first_vertex = ref (-1,(-1,-1)) in
	
	while not !exit do
		let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [Button_down; Key_pressed]
		in
		exit := key = 'a';

		if button then
			let vopt = closest_vertex mouse_x mouse_y (5.*.float_of_int vertex_radius) vlp in
			if vopt <> None then
				if not !selec then (* Selection of the first vertex of the new edge *)
					begin
						first_vertex := option_get vopt;
						let (_,(x,y)) = !first_vertex in
						draw_vertex x y red;
						synchronize ();
						selec := true
					end
				else (* Selection of the second vertex of the new edge *)
					begin
						let (c,_) = !first_vertex in
						let (d,_) = option_get vopt in
						if c<>d then
							(el := (c,d)::!el;
							added_el := (c,d)::!added_el;
							display vlp !el bg2;
							selec := false)
					end;
	done;
	!added_el;;

(* Interface for modifying the placement of the vertices *)
(* Returns a list of all the vertices with their new coordinates *)
(* vlp is the list of present vertices, elp is the list of edges between these vertices *)
let modify_embedding (vlp : vertex_coordinates list) (elp : simple_edge list) : vertex_coordinates list =
	let bg3 () = draw_string "Hold left-click on the vertices to move them around, press [a] to finish" in
	set_window_title "Modifying the embedding";
	display vlp elp bg3;

	let vl = ref vlp in
	let exit = ref false in
	let click = ref false in

	while not !exit do
		let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [Button_down; Key_pressed]
		in
		exit := key = 'a';

		if button then
			let i = (* Index of the vertex on which the user has clicked *)
				let vopt = closest_vertex mouse_x mouse_y (5.*.float_of_int vertex_radius) !vl in
				if vopt <> None
					then (let (i,_) = option_get vopt in i)
					else -1
			in
			
			click := true;
			while !click do
				let {mouse_x; mouse_y; button; keypressed; key} =
					wait_next_event [Button_up; Poll]
				in
				click := button;
				
				if i <> (-1) then (* i = -1 when the click was too far from a vertex *)
					(vl := move_vertex_position i mouse_x mouse_y !vl;
					display !vl elp bg3)
			done;
	done;
	!vl;;

(* Returns the pair (vl,el), where vl is the list of vertices and el the list of the edges *)
let interface () : vertex_coordinates list * simple_edge list =
	init ();
	let avl = add_vertices 0 [] [] in
	let el = add_edges avl [] in
	let vl = modify_embedding avl el in
	close_graph ();
	(vl, el);;

(* let (vl,el) = interface ();; *)

let wait_for_space () : unit =
	let exit = ref false in
	while not !exit do
		let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [Key_pressed]
		in
		exit := key = ' '
	done;;

let print_graph (g : graph) (vl : vertex_coordinates list) : unit =
	let el = graph_to_list_of_edges g in
	init ();
	display vl el (fun () -> ());
	wait_for_space ();
	close_graph ();;


(* Testing *)

let print_colors () =
	let side = 30 in
	let ncol = 8 in
	init();
	let col_tab = [|black; white; red; green; blue; yellow; cyan; magenta|] in
	for i=0 to ncol-1 do
		set_color col_tab.(i);
		fill_rect (side*(i+1)) (size_y() - 70) side side;
		synchronize ();
		for j=i to ncol-1 do
			let (ri,gi,bi) = int_to_rgb col_tab.(i) in
			let (rj,gj,bj) = int_to_rgb col_tab.(j) in
			let (r,g,b) = ((ri+rj)/2, (gi+gj)/2, (bi+bj)/2) in
			print_rgb r g b;

			set_color (rgb r g b);
			fill_rect (side*(j+1)) (size_y() - 70 - (i+1)*side) side side;
			synchronize ()
		done;
	done;
	let actual_col_tab = [|(0,0,0);	(127,127,127); (127,0,0);	(0,127,0); (0,0,127); (127,127,0);	(0,127,127); (127,0,127);
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
		synchronize ()
	done;
	wait_for_space ();
	close_graph ();;
(* print_colors ();; *)
