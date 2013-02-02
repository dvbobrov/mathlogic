open Parser;;
open Prover;;
open Output;;

let contraposition a b =
	let additional_axioms = [Negation(b); Implication(a, b)] in
	let what = Negation(a) in
	let deduction_proof = [(ax9 a b);
		Implication(a, b);
		Implication(Implication(a, Negation(b)), Negation(a));
		(ax1 (Negation(b)) a);
		Negation(b);
		Implication(a, Negation(b));
		Negation(a)] in
	prove what deduction_proof additional_axioms;;

let excluded_middle a =
	let contr1 = contraposition a (Disjunction(a, Negation(a))) in
	let contr2 = contraposition (Negation a) (Disjunction(a, Negation(a))) in
	let t1 = (ax6 a (Negation a)) :: contr1 in
	let t2 = (Implication(Negation(Disjunction(a, Negation(a))), Negation(a))) :: (ax7 a (Negation a)) :: contr2 in
	let t3 = t1 @ t2 in
	t3 @ [
	(Implication(Negation(Disjunction(a, Negation(a))), Negation(Negation(a))));
	ax9 (Negation(Disjunction(a, (Negation a)))) (Negation a);
	Implication(Implication(Negation(Disjunction(a, (Negation a))), Negation(Negation a)), Negation(Negation(Disjunction(a, Negation(a)))));
	Negation(Negation(Disjunction(a, Negation(a))));
	Implication(Negation(Negation(Disjunction(a, Negation(a)))), Disjunction(a, Negation(a)));
	Disjunction(a, Negation(a))
	];;

let rec prove1 what additional_axioms proof =
	debug_output ("prove1 for " ^ (string_of_expression what) ^ "\n"); 
	debug_output "with proof:\n";
	write_expr_list !proof;
	debug_output "and additional axioms:\n";
	write_expr_list additional_axioms;
	match what with
	| Implication (left, right) ->
			let result1 = prove1 left additional_axioms proof in
			let result2 = prove1 right additional_axioms proof in
			if (result2) then (
				debug_output "prove1 impl res2\n";
				proof := !proof @ [(ax1 right left); Implication(left, right)]; 
				true
			)
			else if (result1 && not(result2)) then (
				debug_output "prove1 impl res1\n";
				let pr1 = (ax9 what right) :: (remove_axiom [what; left; right] what [left] []) in
				let n = Negation right in
				let pr2 = (Implication(Implication(Implication(left, right), Negation(right)),
							Negation(Implication(left, right)))) :: (remove_axiom [n] what [n] []) in
				proof := ((!proof @ pr1) @ pr2) @ [Negation (Implication(left, right))];
				false
			) else (
				debug_output "prove1 impl !res2\n";
				let negLeft = Negation left in
				let negRight = Negation right in
				let deductionProof = [
					left; 
					negLeft; 
					negRight; 
					(ax1 negLeft negRight); 
					Implication(negRight, negLeft);
					(ax1 left negRight); 
					Implication(negRight, left); 
					(ax9 negRight left);
					Implication(Implication(negRight, negLeft), Negation(negRight));
					Negation(negRight);
					(ax10 right); 
					right;
					] in
				proof := !proof @ (remove_axiom deductionProof left [negLeft; negRight] []);
				true
			)
	| Negation ex ->
			let result = prove1 ex additional_axioms proof in
			if (not(result)) then true else (
				debug_output ("prove1 negation true");
				let implToSelf = remove_axiom [what] what [] []	in
				proof := (!proof @ ((ax9 what ex) :: (ax1 ex what) :: Implication(Negation ex, ex) ::
					Implication(Implication(Negation ex, Negation ex), Negation(Negation ex)) :: implToSelf)) @ [Negation what];
				false
			)
	| PropositionalVariable _ ->
			let rec iterAxioms ax =
				match ax with
				| [] -> (debug_output "prove1 -> PropositionalVariable -> iterAxioms : nothing"); false
				| x:: ax' ->
						if (x = what) then (debug_output ("Added1 " ^ (string_of_expression what)^"\n"); 
						proof := !proof @ [x]; 
						true) else if (x = (Negation what)) then
							(debug_output ("Added2 " ^ (string_of_expression what)^"\n");
							proof := !proof @ [x];
							false) else iterAxioms ax' in
			iterAxioms additional_axioms
	| _ -> debug_output "prove1 -> smth else"; false;;

module OrderedChar = struct
	type t = char
	let compare = compare
end

module CharSet = Set.Make(OrderedChar)

let prove2 what = 
	let rec getVariables expr set = 
		match expr with
		| PropositionalVariable x -> CharSet.add x set
		| Implication (a, b) | Disjunction (a, b) | Conjunction (a, b) -> 
			let set1 = getVariables a set in
			getVariables b set1
		| Negation a -> getVariables a set
		| Nil -> set 
	in
	let varHolder = CharSet.elements (getVariables what CharSet.empty) in
	let rec prove2Impl vars additional_axioms = 
		match vars with
		| [] -> 
			debug_output ("prove2Impl for []\n");
			write_expr_list additional_axioms;
			let proof = ref [] in
			let res = prove1 what additional_axioms proof in
			if res then !proof else []
		| cur ::vars' ->
			debug_output ("prove2Impl for vars\n");
			write_expr_list additional_axioms;
			let var = PropositionalVariable cur in
			let pr1 = prove2Impl vars' (var :: additional_axioms) in
			let deductionPr1 = remove_axiom pr1 var additional_axioms [] in
			let neg = Negation var in
			let pr2 = prove2Impl vars' (neg :: additional_axioms) in
			let deductionPr2 = remove_axiom pr2 neg additional_axioms [] in
			((deductionPr1 @ deductionPr2) @ (excluded_middle var)) 
			@ [ax8 var neg what; 
			Implication(Implication(neg, what), Implication(Disjunction(var, neg), what));
			Implication(Disjunction(var, neg), what); what]
	in
	prove2Impl varHolder [];; 