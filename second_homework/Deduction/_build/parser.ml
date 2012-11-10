type expression =
	| PropositionalVariable of char
	| Implication of expression * expression
	| Conjunction of expression * expression
	| Disjunction of expression * expression
	| Negation of expression
;;

let parse_expr input_string =
	let rec parse_impl pos =
		(* print_string ("parse_impl " ^ string_of_int(pos) ^ "\n"); *)
		let (p1, r1) = parse_disjunction pos in
		if (String.length input_string > p1 + 2) && (input_string.[p1 + 1] = '-') && (input_string.[p1 + 2] = '>') then
			let (p2, r2) = parse_impl (p1 + 3) in
			(p2, Implication(r1, r2))
		else
			(p1, r1)
	and
	parse_disjunction pos =
		(* print_string ("parse_disj " ^ string_of_int(pos) ^ "\n"); *)
		let (p1, r1) = parse_conjunction pos in
		if (String.length input_string > p1 + 1) && (input_string.[p1 + 1] = '|') then
			let (p2, r2) = parse_disjunction (p1 + 2) in
			(p2, Disjunction(r1, r2))
		else
			(p1, r1)
	and
	parse_conjunction pos =
		(* print_string ("parse_conj " ^ string_of_int(pos) ^ "\n"); *)
		let (p1, r1) = parse_term pos in
		if (String.length input_string > p1 + 1) && (input_string.[p1 + 1] = '&') then
			let (p2, r2) = parse_conjunction (p1 + 2) in
			(p2, Conjunction(r1, r2))
		else
			(p1, r1)
	and
	parse_term pos =
		(* print_string ("parse_term " ^ string_of_int(pos) ^ "\n"); *)
		if (input_string.[pos] >= 'A') && (input_string.[pos] <= 'Z') then begin
			(* print_string "PropVar\n"; *)
			(pos, PropositionalVariable(input_string.[pos])) end
		else if input_string.[pos] = '!' then
			let (p1, r1) = parse_term (pos + 1) in
			(p1, Negation(r1))
		else if input_string.[pos] = '(' then
			let (p1, r1) = parse_impl (pos + 1) in
			if input_string.[p1 + 1] = ')' then
				(p1 + 1, r1)
			else
				failwith ("Syntax error in expression, position: " ^ string_of_int(p1))
		else
			failwith ("Syntax error in expression, position: " ^ string_of_int(pos))
	in
	let (pos, result) =
		try
			parse_impl 0
		with Invalid_argument e ->
				failwith "Syntax error in expression, reached end of string before end of expression"
	in
	if pos = (String.length input_string - 1) then
		result
	else
		failwith ("Syntax error in expression, position: " ^ string_of_int(pos))
;;

let rec string_of_expression expr =
	match expr with
	| PropositionalVariable e1 -> String.make 1 e1
	| Implication (e1, e2) -> "(" ^ (string_of_expression e1) ^ "->" ^ (string_of_expression e2) ^ ")"
	| Disjunction (e1, e2) -> "(" ^ (string_of_expression e1) ^ "|" ^ (string_of_expression e2) ^ ")"
	| Conjunction (e1, e2) -> "(" ^ (string_of_expression e1) ^ "&" ^ (string_of_expression e2) ^ ")"
	| Negation e1 -> "!" ^ (string_of_expression e1);;

let rec list_to_expr_list list =
	match list with
	| x :: xs -> (parse_expr x) :: (list_to_expr_list xs)
	| [] -> [];;

