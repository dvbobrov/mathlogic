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
	let t2 = (ax7 a (Negation a)) :: contr2 in
	let t3 = t1 @ t2 in
	t3 @ [
		ax9 (Negation(Disjunction(a, (Negation a)))) (Negation a);
		Implication(Implication(Negation(Disjunction(a, (Negation a))), Negation(Negation a)), Negation(Negation(Disjunction(a, Negation(a)))));
		Negation(Negation(Disjunction(a, Negation(a))));
		Implication(Negation(Negation(Disjunction(a, Negation(a)))), Disjunction(a, Negation(a)));
		Disjunction(a, Negation(a))
	];; 
