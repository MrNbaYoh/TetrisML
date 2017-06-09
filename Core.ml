directory "TetrisML/";;
#open "graphics";;
#open "random";;
include "GraphicUtils";;
include "InputManager";;
include "TetroBase";;
include "Utils";;
include "Grid";;
include "TetroManager";;
include "TetroRenderer";;
include "GridRenderer";;
include "TextPane";;
include "TetroPane";;
include "GameManager";;

let BACK_COLOR = 0x00c0ff;;
let WIN_H = 900 and WIN_W = 700;;
type state = INGAME | PAUSED | GAME_OVER;;

let init () = 
	open_graph ((string_of_int WIN_W)^"x"^(string_of_int WIN_H));
	random__init (int_of_float (sys__time() *. 1000.0));;
	(* On multiplie par 1000 pour faire passer les chiffres devant la virgule et ainsi obtenir une valeur différente en int pour chaque execution du progamme *)
	
init();;
include "logo";;

let refresh_background () = 
	set_color BACK_COLOR;
	fill_rect 0 0 WIN_W WIN_H;
	draw_image logo 0 (WIN_H - logo_h);
	let y = ref 0
	and l = [|"Z,Q,S,D: move"; "A,E: rotate"; "P: pause"; "R: retry/resume";"M: sound on/off"|] in
	for i = 0 to (vect_length l) - 1 do
		let dx,dy = size_text l.(i) 30 "Serif" in
		draw_text (WIN_W-5-dx) !y l.(i) 30 "Serif" black;
		y := !y + dy; 
	done;;


let refresh_sound_text sound = 
	let str = "Sound: " ^ (if sound then "On " else "Off") in
	let dx,dy = size_text str 30 "Serif" in
	let x = WIN_W-5-dx and y = WIN_H-dy in
	set_color BACK_COLOR;
	fill_rect x y dx dy;
	draw_text x y str 30 "Serif" black;;


refresh_background();;

let g_color = mul_color 0.8 BACK_COLOR;;
let s_pane = {t_p_x=WIN_W*15/16-180; t_p_y=WIN_H*14/16-120; t_p_h=120; t_p_w=180; t_p_text="Score:"; t_p_color=g_color};;
let l_pane = {t_p_x=WIN_W*15/16-180; t_p_y=WIN_H*11/16-120; t_p_h=120; t_p_w=180; t_p_text="Level:"; t_p_color=g_color};;

let t_pane = make_tetro_pane (WIN_W*15/16-180) (WIN_H*8/16-180) 180 180 g_color "Next :";;

let gam = create_game ((WIN_W-NB_COLUMNS*BRICK_SIZE)/8) 0 g_color s_pane l_pane t_pane 1;;
init_game gam;;
refresh_sound_text true;;

let current_state = ref INGAME;;

let game_over () = 
	reset_game gam 1;
	draw_center_text (WIN_W/2) (WIN_H/2) "Game Over !" 72 "Serif" white;
	if gam.sound then
		for i = 0 to 2 do
			sound (330-(i*50)) 200;
		done;;

let pause () = 
	draw_center_text (WIN_W/2) (WIN_H/2) "Game Paused" 72 "Serif" white;;

let ingame () = 
	clear_graph();
	refresh_background();
	refresh_sound_text gam.sound;
	render_all gam;
	gam.last_fall <- sys__time();;

let change_state state = 
	if not !current_state = state then
	begin
		current_state := state;
		match state with
		| INGAME -> ingame();
		| PAUSED -> pause();
		| GAME_OVER -> game_over();
	end;;

let handle_input key = 
	if key = `m` then 
	begin
		gam.sound <- not gam.sound;
		refresh_sound_text gam.sound;
	end
	else
		match !current_state with
		| INGAME -> (game_input key gam; if key = `p` then change_state PAUSED;)
		| PAUSED | GAME_OVER -> if key = `r` then change_state INGAME;;

try
	while true do
		update_input();
		
		if is_key_pressed() then
			let key = get_key() in
				handle_input key;
				
		if !current_state = INGAME then
		begin
			if not (update_game (get_key()) gam) then 
				change_state GAME_OVER;
		end;
	done;
with Exit -> print_string "END";;









