let GRID_BORDER = 5;;
let LEVEL1_FALL_DELAY = 0.8;;
let MAX_LEVEL = 20;;
let MIN_FALL_DELAY = 0.1;;
let D_DELAY = (LEVEL1_FALL_DELAY-.MIN_FALL_DELAY) /. (float_of_int (MAX_LEVEL-1));;
let NB_COLUMNS = 10 and NB_ROWS = 20;;


type Game = {
	grid : Grid;
	grid_back : color;
	mutable tetro : Tetromino;
	mutable next_tetro : Tetromino;
	mutable t_pos_x : int;
	mutable t_pos_y : int;
	mutable fall_delay : float;
	mutable level : int;
	mutable score : int;
	score_pane : TextPane;
	level_pane : TextPane;
	tetro_pane : TetroPane;
	mutable lines : int;
	mutable next_level_nb_lines : int;
	mutable last_fall : float;
	mutable sound : bool;
};;

type move = DOWN | LEFT | RIGHT;;

let create_game g_x g_y g_back s_p l_p t_p start_level = 
	if start_level = 0 || start_level > 20 then failwith "Illegal value for level";
	{
		grid = make_playable_grid NB_ROWS NB_COLUMNS (GRID_BORDER+g_x) (GRID_BORDER+g_y);
		grid_back = g_back;
		tetro = make_random_tetro 0 0;
		next_tetro = make_random_tetro 0 0;
		t_pos_x = 0;
		t_pos_y = 0;
		fall_delay = LEVEL1_FALL_DELAY -. D_DELAY *. (if start_level <= MAX_LEVEL then (float_of_int (start_level-1)) else (float_of_int (MAX_LEVEL-1)));
		level = start_level;
		score = 0;
		score_pane = s_p;
		level_pane = l_p;
		tetro_pane = t_p;
		lines = 0;
		next_level_nb_lines = min 100 (start_level*10);
		last_fall = 0.0;
		sound = true;
	};;

let spawn_tetro game = 
	game.tetro <- game.next_tetro;
	game.next_tetro <- make_random_tetro 0 0;
	(* On centre en x *)
	game.t_pos_x <- game.grid.margin+(NB_COLUMNS-game.tetro.t_base.dim)/2;
	(* On positionne le tetromino de façon à avoir le coin supérieur 
		gauche au-dessus de la partie jouable car tous les tetrominos on une
		première ligne vide, ainsi les premières birques remplies se trouvent
		collées au bord supérieur de la partie jouable *)
	game.t_pos_y <- game.grid.margin+NB_ROWS-game.tetro.t_base.dim+1;
	(* On aligne finalement le tetromino *)			
	align_tetro_at_grid_pos game.t_pos_x game.t_pos_y game.grid game.tetro;;

let is_move_possible move game = 
	let dx = match move with
				| LEFT -> -1;
				| RIGHT -> 1;
				| _ -> 0
	and dy = if move = DOWN then -1 else 0 in
	not check_collision (game.t_pos_x+dx) (game.t_pos_y+dy) game.grid game.tetro;;

let move_tetro move game = 
	erase_tetro game.tetro game.grid_back;
	let dx = match move with
				| LEFT -> -1;
				| RIGHT -> 1;
				| _ -> 0
	and dy = if move = DOWN then -1 else 0 in
	game.t_pos_x <- game.t_pos_x+dx;
	game.t_pos_y <- game.t_pos_y+dy;
	align_tetro_at_grid_pos game.t_pos_x game.t_pos_y game.grid game.tetro;
	draw_tetro game.tetro;;

let move_if_possible move game = 
	if is_move_possible move game then move_tetro move game;;

let inv_rot = fun
| R_RIGHT -> R_LEFT;
| _ -> R_RIGHT;;

let rotate_if_possible rot game = 
	rotate_tetro game.tetro rot;
	if check_collision game.t_pos_x game.t_pos_y game.grid game.tetro then
		rotate_tetro game.tetro (inv_rot rot)
	else 
	begin
		rotate_tetro game.tetro (inv_rot rot);
		erase_tetro game.tetro game.grid_back;
		rotate_tetro game.tetro rot;
		draw_tetro game.tetro;
	end;;

let game_input key game = 
		match key with
		| `e` -> rotate_if_possible R_RIGHT game;
		| `a` -> rotate_if_possible R_LEFT game;
		| `s` -> move_if_possible DOWN game;
		| `q` -> move_if_possible LEFT game;
		| `d` -> move_if_possible RIGHT game;
		| `x` -> erase_grid game.grid game.grid_back;
		| _ -> ();;


let render_grid game = 
		erase_grid game.grid game.grid_back;
		draw_playable_zone game.grid;
		draw_grid_outlines black 5 game.grid;;

let render_all game = 
		render_grid game;
		draw_tetro game.tetro;
		update_text_pane game.score_pane game.score;
		update_text_pane game.level_pane game.level;
		update_tetro_pane game.tetro_pane game.next_tetro;;

let incr_score game nb_lines = 
	let points = match nb_lines with
					 | 1 -> ref 40;
					 | 2 -> ref 100;
					 | 3 -> ref 300;
					 | 4 -> ref 1200;
					 | _ -> failwith "Cannot remove more than 4 lines!"
	in game.score <- game.score + !points * game.level;
	update_text_pane game.score_pane game.score;;

let incr_level game = 
	if game.level < MAX_LEVEL then 
	begin
		game.level <- game.level+1;
		game.next_level_nb_lines <- game.next_level_nb_lines + 10;
		game.fall_delay <- game.fall_delay -. D_DELAY;
		update_text_pane game.level_pane game.level;
	end;;
	
let incr_nb_lines game nb_lines = 
	if nb_lines > 4 then failwith "Cannot remove more than 4 lines!";
	game.lines <- game.lines + nb_lines;
	if game.lines >= game.next_level_nb_lines then incr_level game;;
	
(* Joue un son en fonction du nb de lignes complètes *)
let drop_brick game = 
	let need_render_grid = ref false in
			insert_tetro game.t_pos_x game.t_pos_y game.grid game.tetro;
			let nb_lines = remove_complete_lines game.grid game.t_pos_y game.tetro.t_base.dim in
			if nb_lines > 0 then 
			begin
				incr_score game nb_lines;
				incr_nb_lines game nb_lines;
				if game.sound then
				begin
					for i = 0 to nb_lines-1 do
					let freq = (390+i*50) - (if i = 4 then 20 else 0) in
						sound freq 100;
					done;
				end;
				need_render_grid := true;
			end
			else
				if game.sound then sound 1976 100;
				(* Joue un son différent lorsque le bloc est posé mais aucune ligne n'est complète *)
		
			if !need_render_grid then render_grid game;;			

let spawn_tetro_is_game_over game = 
	spawn_tetro game;
	check_collision game.t_pos_x game.t_pos_y game.grid game.tetro;;
	
		
let update_game key game = 
	try
	if (sys__time()-. game.last_fall) > game.fall_delay then
	begin
		if not is_move_possible DOWN game then
		begin
			drop_brick game;
			if spawn_tetro_is_game_over game then
				raise Exit;
			draw_tetro game.tetro;
			update_tetro_pane game.tetro_pane game.next_tetro;
		end
		else
		begin 
			move_tetro DOWN game;
		end;
	game.last_fall <- sys__time();
	end;
	true
	with Exit -> false;;

let init_game game = 
	spawn_tetro game;
	render_all game;;
	

let reset_game game start_level = 
	if start_level = 0 || start_level > 20 then failwith "Illegal value for level";
	clear_playable_zone game.grid;
	game.lines <- 0;
	game.next_level_nb_lines <- min 100 (start_level*10);
	game.score <- 0;
	game.level <- start_level;
	game.fall_delay <- LEVEL1_FALL_DELAY -. D_DELAY *. (if start_level <= MAX_LEVEL then (float_of_int (start_level-1)) else (float_of_int (MAX_LEVEL-1)));
	game.last_fall <- 0.0;
	spawn_tetro game;;