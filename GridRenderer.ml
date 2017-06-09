let draw_playable_zone grid = 
	let h = vect_length grid.content
	and w = vect_length grid.content.(0)
	and x = grid.grid_x + grid.margin*BRICK_SIZE 
	and y = grid.grid_y + grid.margin*BRICK_SIZE
	and margin = grid.margin in
	for i = margin to h-1-margin do
		for j = margin to w-1-margin do
			match grid.content.(i).(j) with
			| Empty -> ();
			| Filled col -> draw_brick (x+(j-margin)*BRICK_SIZE) (y+(i-margin)*BRICK_SIZE) col;
		done;
	done;;
	

let draw_grid_outlines color border_size grid = 
	let margin = grid.margin*BRICK_SIZE in
	let h = BRICK_SIZE*(vect_length grid.content) - 2*margin
	and w = BRICK_SIZE*(vect_length grid.content.(0)) - 2*margin in
	draw_outline_rect (grid.grid_x+margin) (grid.grid_y+margin) h w border_size color;;

let erase_grid grid color =
	let h = ((vect_length grid.content)-2*grid.margin)*BRICK_SIZE
	and w = ((vect_length grid.content.(0))-2*grid.margin)*BRICK_SIZE in 
	set_color color;
	fill_rect (grid.grid_x+grid.margin*BRICK_SIZE) (grid.grid_y+grid.margin*BRICK_SIZE) w h;; 