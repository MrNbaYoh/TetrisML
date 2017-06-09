#open "graphics";;

let BRICK_SIZE = 40;;

type BaseGrid == bool vect vect;;
type BaseTetromino = {
	dim : int;
	states : BaseGrid vect;
	color : color;
};;

type Tetromino = {
	mutable tetro_x : int;
	mutable tetro_y : int;
	t_base : BaseTetromino;
	mutable state : int
};;

type rotation = R_LEFT | R_RIGHT;;

(* Effectue la rotation d'une matrice *)
let rotate rot mat h w = 
	let result = make_matrix w h false in
	for i = 0 to h-1 do
		for j = 0 to w-1 do
			if rot = R_LEFT then
				result.(j).(i) <- mat.(i).(w-1-j)
			else
				result.(j).(i) <- mat.(h-1-i).(j);
		done;
	done;
	result;;

let filled_line l b = make_vect l b;;
let line_single l p b1 b2 = 
	let res = make_vect l b2 in res.(p) <- b1; res;;

let T_SQUARE = 
	let base = [|
					filled_line 4 false;
					[|false; true; true; false|];
					[|false; true; true; false|];
					filled_line 4 false
		   	  |]
	in
	{ 
		dim = 4;
		states = filled_line 4 base;
		color = yellow;
	};;

let T_LINE = 
	let base = [|
					filled_line 4 false;
					filled_line 4 false;
					filled_line 4 true;
					filled_line 4 false
			 	  |]
	in
	let second = rotate R_LEFT base 4 4 in
	{
		dim = 4;
		states = [|base; second; base; second|];
		color = cyan;
	};;

let T_T = 
	let base = [|
					line_single 3 1 true false;
					filled_line 3 true;
					filled_line 3 false
			 	  |]
	in 
	let second = rotate R_LEFT base 3 3 in
	{
		dim = 3;
		states = [|base; second; rotate R_LEFT second 3 3; rotate R_RIGHT base 3 3|];
		color = magenta;
	};;

let T_L revert = 
	let base = [|
					line_single 3 (if revert then 2 else 0;) true false;
					filled_line 3 true;
					filled_line 3 false
				  |]
	in
	let second = rotate R_LEFT base 3 3 in
	{
		dim = 3;
		states = [|base; second; rotate R_LEFT second 3 3; rotate R_RIGHT base 3 3|];
		color = if revert then blue else rgb 255 128 0;
	};;

let T_S revert = 
	let base = [|
					line_single 3 (if revert then 2 else 0;) false true;
					line_single 3 (if revert then 0 else 2;) false true;
					filled_line 3 false
			     |] 
	in
	let second = rotate (if revert then R_RIGHT else R_LEFT) base 3 3 in
	{
		dim = 3;
		states = [|base; second; base; second|];
		color = if revert then green else red;
	};;

let tetro_bases = [| T_SQUARE; T_LINE; T_T; T_S true; T_S false; T_L true; T_L false; |];;