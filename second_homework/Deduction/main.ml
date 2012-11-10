open Reader;;
open Prover;;
open Output;;

let (what, deduction_proof, additional_axioms) = get_problem "input.txt" in
let proof = prove what deduction_proof additional_axioms in
write_proof_to_file proof "output.txt" ;;



