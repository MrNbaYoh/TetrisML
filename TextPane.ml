type TextPane =
{
	t_p_x : int;
	t_p_y : int;
	t_p_h : int;
	t_p_w : int;
	t_p_text : string;
	t_p_color : color;
};;

(* TODO : mieux dessiner le texte, par rapport au coin sup gauche*)
let update_text_pane pane val = 
	set_color pane.t_p_color;
	fill_rect pane.t_p_x pane.t_p_y pane.t_p_w pane.t_p_h;
	draw_outline_rect pane.t_p_x pane.t_p_y pane.t_p_h pane.t_p_w 5 black;
	draw_text (pane.t_p_x+(pane.t_p_w/128)) (pane.t_p_y+(pane.t_p_h*96/128)) pane.t_p_text 30 "Serif" black;
	draw_center_text (pane.t_p_x+(pane.t_p_w/2)) (pane.t_p_y+(pane.t_p_h/2)) (string_of_int val) 30 "Serif" black;;
	