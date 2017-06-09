directory "TetrisML/";;
include "TetroBase";;
#open "graphics";;

(* La marge de base entre la partie jouable de la grille et ses vraies dimensions *)
(* Cela correspond à la largeur du plus grand rectangle vide qu'une pièce peut contenir + 1*)
(* Cette marge permet de placer les tetrominos dans la grille même si leur partie vide dépasse la zone jouable *)
(* Exemple : la ligne 
	--*-
	--*-
	--*-
	--*-
	la partie vide à gauche peut sortir de la grille jouable
*)
(* La "+1" est nécessaire pour éviter les erreurs dans le check des collisions
	par exemple avec cette ligne, la vérification d'une collision en déplaçant à gauche 
	provoquerait une erreur avec une marge de 2 
*)
let MARGIN = 3;;

(* Une brique est soit vide soit remplie d'un couleur *)
type Brick = Empty | Filled of color;;
type Grid = {
	grid_x : int;
	grid_y : int;
	content : Brick vect vect;
	margin : int;
};;

(* Retire une ligne de la grille *)
let remove_line grid l =
	let h = vect_length grid.content in
	if l < 0 || l >= h then failwith "Line index out of bounds in remove_line !";
	for i = l to h-2 do
		grid.content.(i) <- grid.content.(i+1);
	done;
	grid.content.(h-1-grid.margin) <- filled_line (vect_length grid.content.(0)) Empty;
	let w = vect_length grid.content.(0) in
	for i = 0 to grid.margin-1 do
		grid.content.(h-1-grid.margin).(i) <- Filled black;
		grid.content.(h-1-grid.margin).(w-1-i) <- Filled black;
	done;;
	
let is_line_complete grid l = 
	try
		let w = vect_length grid.content.(l) in
		for i = grid.margin to w-1-grid.margin do
			if grid.content.(l).(i) = Empty || grid.content.(l).(i) = Filled black then raise Exit;
		done;
	true
	with Exit -> false;;

(* Insère le contenu d'un tetromino dans la grille *)
let insert_tetro x y grid tetro = 
	let dim = tetro.t_base.dim-1
	and array = tetro.t_base.states.(tetro.state) in
	for i = 0 to dim do
		for j = 0 to dim do
			match array.(i).(j) with
			| true -> grid.content.(y+i).(x+j) <- Filled tetro.t_base.color;
			| _ -> ();
		done;
	done;;

(* Vide la partie jouable d'une grille *)
let clear_playable_zone grid =
	let margin = grid.margin 
	and h = vect_length grid.content
	and w = vect_length grid.content.(0) in 
	for i = grid.margin to h-1-margin do
		for j = grid.margin to w-1-margin do
			grid.content.(i).(j) <- Empty;
		done;
	done;;

(* Construit une grille dont la partie jouable est positionnée aux coordonnées (x,y) du graphe *)
let make_playable_grid h w x y =
	let res = 
	{
		grid_x = x-MARGIN*BRICK_SIZE;
		grid_y = y-MARGIN*BRICK_SIZE;
		content = make_matrix (h+2*MARGIN) (w+2*MARGIN) (Filled black);
		margin = MARGIN;
	} in
	clear_playable_zone res;
	res;; 

(* Verifie si il y a une intersection entre le tetromino et la grille donnés, retourne vrai si c'est le cas *)
let check_collision x y grid tetro =
	try
		let dim = tetro.t_base.dim
		and tetro_grid = tetro.t_base.states.(tetro.state) in
			for i = 0 to dim-1 do
				for j = 0 to dim-1 do
					if not grid.content.(y+i).(x+j) = Empty && tetro_grid.(i).(j) then
					raise Exit;
				done;
			done;
		false
	with Exit -> true;;	 

(* Efface les lignes complètes dont l'indices est compris entre l et 
	l+dim-1, retourne le nombre de lignes éffacées *)
let remove_complete_lines grid l dim =
	let res = ref 0 in
	for i = dim-1 downto 0 do
		if is_line_complete grid (l+i) then
		begin
			remove_line grid (l+i);
			incr res;
		end;
	done;
	!res;;
 

