open Parser ;;

let write_proof_to_file proof filename = 
	let fo = open_out filename in
	let rec write_to_file list = 
		match list with
			| x :: xs -> output_string fo ((string_of_expression x) ^ "\n"); write_to_file xs
			| [] -> ()
	in
	write_to_file proof ;;


let rec write_expr_list list = 
	match list with
		| x::xs -> print_string ((string_of_expression x)^"\n"); write_expr_list xs
		| [] -> print_string "---\n"
;;
