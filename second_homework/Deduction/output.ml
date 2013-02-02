open Parser ;;

let debug = false;;

let write_string_to_file str filename = 
	let fo = open_out filename in
	output_string fo str;;
	
let debug_output str = if debug then print_string str else ();;	

let write_proof_to_file proof filename = 
	let fo = open_out filename in
	let rec write_to_file list = 
		match list with
			| x :: xs -> output_string fo ((string_of_expression x) ^ "\n"); write_to_file xs
			| [] -> ()
	in
	write_to_file proof ;;


let rec write_expr_list list = 
	if debug then
	match list with
		| x::xs -> print_string ((string_of_expression x)^"\n"); write_expr_list xs
		| [] -> print_string "---\n"
	else ()
;;
