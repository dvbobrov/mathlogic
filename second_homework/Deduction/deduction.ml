open Reader;;
open Prover;;
open Output;;
open Parser;;


let (what, deduction_proof, additional_axioms) = get_problem "deduction_input.txt" in
(* (* debug_output ("What:"^(string_of_expression what)^"\n"); *)              *)
(* (* write_expr_list deduction_proof;                         *)              *)
(* (* write_expr_list additional_axioms;                       *)              *)
let proof = prove what deduction_proof additional_axioms in
write_proof_to_file proof "deduction_output.txt" ;;