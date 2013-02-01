open Reader;;
open Prover;;
open Output;;
open Parser;;
open ProverSecond;;


(* let (what, deduction_proof, additional_axioms) = get_problem "input.txt" in       *)
(* (* (* print_string ("What:"^(string_of_expression what)^"\n"); *)              *) *)
(* (* (* write_expr_list deduction_proof;                         *)              *) *)
(* (* (* write_expr_list additional_axioms;                       *)              *) *)
(* let proof = prove what deduction_proof additional_axioms in                       *)
(* write_proof_to_file proof "output.txt" ;;                                         *)
write_proof_to_file (excluded_middle (PropositionalVariable 'A')) "output.txt";;