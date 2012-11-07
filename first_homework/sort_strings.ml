let lines = ref [];;

let fi = open_in "input.txt" in
try
	while true do
		lines := (input_line fi) :: !lines
	done;
with End_of_file ->
		close_in fi;;

lines := List.sort String.compare !lines;;

let output_list l =
	let rec output_list_to_channel l channel =
		match l with
		| [] -> ()
		| el :: tail -> output_string channel (el ^ "\n"); output_list_to_channel tail channel in
	let fo = open_out "output.txt" in
	output_list_to_channel l fo;
	close_out fo in
output_list !lines;;