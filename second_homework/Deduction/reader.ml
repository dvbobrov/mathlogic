open Parser;;

let read_lines filename =
	let lines = ref [] in
	let fi = open_in filename in
	try
		while true do
			let line = input_line fi in
			(* print_string ("Line: "^line^"\n"); *)
			if not(line = "") then
				lines := line :: !lines
		done; ("", [])
	with End_of_file ->
			close_in fi;
	match !lines with
	| x:: xs -> (x, List.rev xs)
	| [] -> failwith "File is empty"
;;

let read_first_line filename = 
	let fi = open_in filename in
	try
		input_line fi
	with End_of_file ->
		failwith "File is empty";;

let parse_problem str =
	let replaced = Str.global_replace (Str.regexp " ") "" str in
	if not(Str.string_match (Str.regexp "^\\([A-Z|&>()-]+\\(,[A-Z|&>()-]+\\)*\\)?:-[A-Z|&>()-]+$") replaced 0) then
		raise (Failure "String does not match the rule")
	else
		let splitted = List.rev (Str.split (Str.regexp "\\(,\\|:-\\)") replaced) in
		match splitted with
		| x:: xs -> (x, xs)
		| [] -> failwith "O_o"
;;

let get_problem filename =
	let (problem, proof) = read_lines filename in
	let (what_to_prove, additional_axioms) = parse_problem problem in
	((parse_expr what_to_prove), (list_to_expr_list proof), (list_to_expr_list additional_axioms));;