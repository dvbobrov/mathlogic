open Parser ;;
open Reader ;;
open Prover ;;
open Output ;;

let failed_count = ref 0;;

let test test_num func =
	print_string ("Test" ^ string_of_int(test_num) ^ "\n");
	try
		func ();
		print_string "Test passed\n";
		();
	with Assert_failure e ->
			failed_count := !failed_count + 1;
			print_string "Test failed\n";
			();;

(* Propositional vars *)
test 1 (fun () ->
				assert (parse_expr "A" = PropositionalVariable('A')) ;
				assert (parse_expr "A" != PropositionalVariable('B')) ;
				assert ("A" = string_of_expression (PropositionalVariable('A')));)
;;

(* Implication *)
test 2 (fun () ->
				assert (parse_expr "A->B" = Implication(PropositionalVariable('A'), PropositionalVariable('B')));
				assert (parse_expr "A->B->C" =
					Implication(
						PropositionalVariable('A'),
						Implication(PropositionalVariable('B'), PropositionalVariable('C')))) ;
				assert ("(A->B)" = string_of_expression (Implication(PropositionalVariable('A'), PropositionalVariable('B')))) ;);;

(* Disjunction *)
test 3 (fun () ->
				assert (parse_expr "A|B" = Disjunction(PropositionalVariable('A'), PropositionalVariable('B'))) ;
				assert ("(A|B)" = string_of_expression (Disjunction(PropositionalVariable('A'), PropositionalVariable('B')))) ;
	);;

(* Conjunction *)
test 4 (fun () ->
				assert (parse_expr "A&B" = Conjunction(PropositionalVariable('A'), PropositionalVariable('B'))) ;
				assert ("(A&B)" = string_of_expression (Conjunction(PropositionalVariable('A'), PropositionalVariable('B')))) ;
	);;

(* Negation *)
test 5 (fun () ->
				assert (parse_expr "!A" = Negation(PropositionalVariable('A'))) ;
				assert ("!A" = string_of_expression (Negation(PropositionalVariable('A')))) ;
	);;

(* Brackets *)
test 6 (fun () ->
				assert (parse_expr "(A->B)->C" =
					Implication(
						Implication(PropositionalVariable('A'), PropositionalVariable('B')),
						PropositionalVariable('C')
					)) ;
	);;

(* Combination *)
test 7 (fun () ->
				assert (parse_expr "A->(!(B&C|D->A)|A&!C->!X)" =
					Implication(
						PropositionalVariable('A'),
						Implication(
							Disjunction(
								Negation(
									Implication(
										Disjunction(
											Conjunction(
												PropositionalVariable('B'),
												PropositionalVariable('C')),
											PropositionalVariable('D')),
										PropositionalVariable('A')
									)
								),
								Conjunction(
									PropositionalVariable('A'),
									Negation(PropositionalVariable('C')))
							),
							Negation(PropositionalVariable('X'))
						)
					)
				);
	);;

(* Problem parser *)
test 8 (fun () ->
				assert (parse_problem "(A->B) & (C->D), B -> C :- A -> C" = ("A->C", ["B->C"; "(A->B)&(C->D)"]));
	);;

(* Modus Ponens *)
test 9 (fun () ->
	assert (modus_ponens (parse_expr "(A->B)->(B->C)") (parse_expr "A->B") = parse_expr "B->C")
	);;

test 10 (fun () ->
	assert (modus_ponens (parse_expr "A->B") (parse_expr "(A->B)->(B->C)") = parse_expr "B->C")
	);;

(* Axiom cases *)
test 11 (fun () ->
	assert(prove_axiom_case (parse_expr "A->B") (parse_expr "A->C") = [parse_expr "(A->B)->(A->C)->(A->B)"; parse_expr "(A->C)->(A->B)"])
	);;

(* MP cases *)
test 12 (fun () -> 
	let prev = list_to_expr_list ["A->B->A"; "A"; "B->A"; "A->B->C"; "(A->B->C)->B->(A->B->C)"; "B->(A->B->C)"] in
	let proof = try_modus_ponens (parse_expr "B->C") (parse_expr "B") prev [] in
	assert(proof = list_to_expr_list ["(B->A)->(B->A->(B->C))->(B->(B->C))"; 
		"(B->A->(B->C))->(B->(B->C))";
		"B->(B->C)"])
	);;

(* Implication to self *)
test 13 (fun () ->
	let expr = parse_expr "B" in
	let proof = make_proof_from_deduction expr expr [] [] in
	assert (proof = list_to_expr_list ["B->B->B"; "(B->(B->B))->(B->((B->B)->B))->(B->B)"; "(B->((B->B)->B))->(B->B)"; "(B->((B->B)->B))"; "B->B"])
	);;

print_string ("Testing finished. Failed: " ^ (string_of_int !failed_count));;