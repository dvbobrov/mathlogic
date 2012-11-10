open Parser;;

let ax1 a b = Implication(a, Implication(b, a)) ;;
let ax2 a b c = Implication(Implication(a, b), Implication(Implication(a, Implication(b, c)), Implication(a, c)));;
let ax3 a b = Implication(a, Implication(b, Conjunction(a, b)));;
let ax4 a b = Implication(Conjunction(a, b), a);;
let ax5 a b = Implication(Conjunction(a, b), b);;
let ax6 a b = Implication(a, Disjunction(a, b));;
let ax7 a b = Implication(b, Disjunction(a, b));;
let ax8 a b c = Implication(Implication(a, c), Implication(Implication(b, c), Implication(Disjunction(a, b), c)));;
let ax9 a b = Implication(Implication(a, b), Implication(Implication(a, Negation(b)), Negation(a)));;
let ax10 a = Implication(Negation(Negation(a)), a);;

let modus_ponens a b =
	match a with
	| Implication(e1, e2) when e1 = b -> e2
	| _ -> match b with
			| Implication(e1, e2) when e1 = a -> e2
			| _ -> failwith "Modus ponens is impossible"

let prove_axiom_case expr cur_axiom =
	let e1 = ax1 expr cur_axiom in
	let e2 = modus_ponens e1 expr in
	[e1; e2];;

let make_proof_from_deduction expr cur_axiom additional_axioms previous_statements =
	match expr with
	| x when x = cur_axiom -> (let e1 = ax1 expr expr in
				let e2 = ax2 expr (Implication(expr, expr)) expr in
				let e3 = modus_ponens e1 e2 in
				let e4 = ax1 expr (Implication(expr, expr)) in
				let e5 = modus_ponens e4 e3 in
				previous_statements @ [e1; e2; e3; e4; e5])
	| Implication(a, Implication(b, c)) 
	when a = c -> 
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| Implication(Implication(a, b), Implication(Implication(a1, Implication(b1, c1)), Implication(a2, c2))) 
	when (a = a1) && (a = a2) && (b = b1) && (c1 = c2) -> 
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| Implication(a, Implication(b, Conjunction(a1, b1))) 
	when (a = a1) && (b = b1) -> 
		previous_statements @ (prove_axiom_case expr cur_axiom)
	|	Implication(Conjunction(a, b), a1) 
	when a = a1 ->	
		previous_statements @ (prove_axiom_case expr cur_axiom)
	|	Implication(Conjunction(a, b), b1) 
	when b = b1 -> 
		previous_statements @ (prove_axiom_case expr cur_axiom)
	|	Implication(a, Disjunction(a1, b)) 
	when a = a1 ->
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| Implication(b, Disjunction(a, b1))
	when b = b1 ->
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| Implication(Implication(a, c), Implication(Implication(b1, c1), Implication(Disjunction(a2, b2), c2)))
	when (a = a2) && (b1 = b2) && (c = c1) && (c = c2) ->
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| Implication(Implication(a, b), Implication(Implication(a1, Negation(b1)), Negation(a2))) 
	when (a = a1) && (a = a2) && (b = b1) ->
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| Implication(Negation(Negation(a)), a1) 
	when a = a1 ->
		previous_statements @ (prove_axiom_case expr cur_axiom)
	| other ->
		let rec try_additional_axioms axioms = 
			match axioms with
				| x::xs -> if x = expr then prove_axiom_case expr cur_axiom else try_additional_axioms xs
				| [] -> []
		in
		let l = try_additional_axioms additional_axioms in
		if not(l = []) then previous_statements @ l else
			[]

;;

let prove what deduction_proof additional_axioms = [];;

