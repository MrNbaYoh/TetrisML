let make_tetro x y b =
	{
	  tetro_x = x;
	  tetro_y = y;
	  t_base = b;
	  state = 0
	};;

let make_random_tetro x y = make_tetro x y (random_value tetro_bases);;

(* Aligne la position d'un tetromino par rapport � une position donn�e dans une grille *)
let align_tetro_at_grid_pos x y grid tetro = 
	tetro.tetro_x <- grid.grid_x + BRICK_SIZE*x;
	tetro.tetro_y <- grid.grid_y + BRICK_SIZE*y;;

(* Effectue la rotation d'un tetromino.
	La repr�sentation de la grille des tetrominos �tant "� l'envers" 
	on inverse ici le sens de rotation effectu� pour avoir le bon r�sultat 
	� l'�cran
*)
let rotate_tetro tetro rot = 
	if rot = R_LEFT then 
		tetro.state <- (tetro.state+3) mod 4
	else
		tetro.state <- (tetro.state+1) mod 4;;