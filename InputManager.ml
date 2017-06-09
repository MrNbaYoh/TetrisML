type input_status = { 
mutable mouse_pos : int*int;
mutable button : bool;
mutable keypressed : bool;
mutable key : char; };;

let status = {
mouse_pos=(-1,-1);
button=false;
keypressed=false;
key=` `;};;

let update_input () = 
	status.mouse_pos <- mouse_pos();
	status.button <- button_down();
	let test = wait_next_event [Poll] in
	status.keypressed <- true;
	if key_pressed() then
		status.key <- read_key()
	else
		status.key <- ` `;
	;;
		
let get_key () = status.key;;
let is_key_pressed () = status.keypressed;;

let get_mouse_pos () = status.mouse_pos;;
let is_button_down () = status.button;;