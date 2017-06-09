type TetroPane = 
{
	g_p_x : int;
	g_p_y : int;
	g_p_h : int;
	g_p_w : int;
	g_p_text : string;
	g_p_back : color;
};;

let make_tetro_pane x y h w color text = 
	if h < 4*BRICK_SIZE || w < 4*BRICK_SIZE then
		failwith "Grid pane too small!";
	{
		g_p_x = x;
		g_p_y = y;
		g_p_h = h;
		g_p_w = w;
		g_p_text = text;
		g_p_back = color;
	};;

(* /!\ Modifie la position du tetro /!\*)
let update_tetro_pane pane tetro = 
	set_color pane.g_p_back;
	fill_rect pane.g_p_x pane.g_p_y pane.g_p_w pane.g_p_h;
	tetro.tetro_x <- pane.g_p_x+(pane.g_p_w-tetro.t_base.dim*BRICK_SIZE)/2;
	tetro.tetro_y <- pane.g_p_y+(pane.g_p_h-tetro.t_base.dim*BRICK_SIZE)/2;
	draw_tetro tetro;
	
	draw_text (pane.g_p_x+(pane.g_p_w/128)) (pane.g_p_y+(pane.g_p_h*96/128)) pane.g_p_text 30 "Serif" black;
	
	draw_outline_rect pane.g_p_x pane.g_p_y pane.g_p_h pane.g_p_w 5 black;;

