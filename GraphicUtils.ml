#open "graphics";;

(* Dessine un simple carrŽ (rempli) *)
let draw_square x y size color = 
	set_color color;
	fill_rect x y size size;;

(* Dessine un carrŽ (rempli) avec une bordure *)
let draw_square_border x y size margin b_color f_color = 
	draw_square x y size b_color;
	draw_square (x+margin) (y+margin) (size-2*margin) f_color;;

(* Dessine une grille *)
let draw_grid x y h w size color = 
	let cur_y = ref y in
	for i = 1 to h do
		let cur_x = ref x in
		for j = 1 to w do
			draw_square_border !cur_x !cur_y size 1 black color;
			cur_x := !cur_x + size;
		done;
		cur_y := !cur_y + size;
	done;;

(* Nettoie le contenu de la fentre et la remplie de la louleur donnŽe *)
let clear_screen color =
	clear_graph();
	set_color color;
	fill_rect 0 0 (size_x()) (size_y());;

(* Dessine du texte *)
let draw_text x y str size font color = 
	moveto x y;
	set_color color;
	set_font font;
	set_text_size size;
	draw_string str;;
	
let draw_center_text x y str size font color = 
	(*moveto 0 0;*)
	set_color color;
	set_font font;
	set_text_size size;
	let w,h = text_size str in
	moveto (x-w/2) (y-h/2);
	draw_string str;;
	
let size_text str size font = 
	set_font font;
	set_text_size size;
	text_size str;;

(* Dessine les bordures intŽrieures d'un rectangle *)
let draw_inline_rect x y h w margin color =
	set_color color;
	fill_rect x y w margin;
	fill_rect (x+w-margin) y margin h;
	fill_rect x (y+h-margin) w margin;
	fill_rect x y margin h;;
	(*let image = get_image (x+margin) (y+margin) (w-2*margin) (h-2*margin) in
	fill_rect x y w h;
	draw_image image (x+margin) (y+margin);;*)

(* Dessine les bordures extŽrieures d'un rectangle *)
let draw_outline_rect x y h w border_size color =
	draw_inline_rect (x-border_size) (y-border_size) (h+2*border_size) (w+2*border_size) border_size color;;

let color_to_rgb color = 
	(color lsr 0x10 land 0XFF, color lsr 0x8 land 0xFF, color land 0xFF);;

let cap_255 val = match val with
| v when v > 255 -> 255;
| _ -> val;;

let mul_color coeff color = let r,g,b = color_to_rgb color in
	rgb (cap_255 (int_of_float ((float_of_int r)*.coeff))) 
		 (cap_255 (int_of_float ((float_of_int g)*.coeff)))
		 (cap_255 (int_of_float ((float_of_int b)*.coeff)));;
let make_darker = mul_color 0.5;;