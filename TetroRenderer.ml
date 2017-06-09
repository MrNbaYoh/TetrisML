let BRICK_BORDER_SIZE = BRICK_SIZE/4;;

(* Dessine une "brique", un carré avec un contour de couleur plus foncée *)
let draw_brick x y color =
	draw_square_border x y BRICK_SIZE BRICK_BORDER_SIZE (make_darker color) color;;

let draw_tetro tetro = 
	let dim = tetro.t_base.dim
	and x = tetro.tetro_x
	and y = tetro.tetro_y
	and array = tetro.t_base.states.(tetro.state) in
	for i = 0 to dim-1 do
		for j = 0 to dim-1 do
			match array.(i).(j) with
			| true -> draw_brick (x+j*BRICK_SIZE) (y+i*BRICK_SIZE) tetro.t_base.color;
			| _ -> ();
		done;
	done;;
	
let erase_tetro tetro color = 
	for i = 0 to tetro.t_base.dim-1 do
		for j = 0 to tetro.t_base.dim-1 do
			if tetro.t_base.states.(tetro.state).(i).(j) then
				draw_square (tetro.tetro_x+j*BRICK_SIZE) (tetro.tetro_y+i*BRICK_SIZE) BRICK_SIZE color;
		done;
	done;;