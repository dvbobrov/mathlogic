type expression = 
	| PropositionalVariable of string
	| Implication of expression * expression
	| Conjunction of expression * expression
	| Disjunction of expression * expression
	| Negation of expression 
	;;

