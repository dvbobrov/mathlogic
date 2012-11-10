open Parser ;;

let write_proof_to_file proof filename = 
	let fo = open_out filename in
	let rec write_to_file list = 
		match list with
			| x :: xs -> output_string fo (string_of_expression x); write_to_file xs
			| [] -> ()
	in
	write_to_file proof ;;
