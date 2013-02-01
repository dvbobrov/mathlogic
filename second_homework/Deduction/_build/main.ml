open Reader;;
open Prover;;
open Output;;
open Parser;;
open ProverSecond;;


let line = read_first_line "completeness_input.txt" in
	write_proof_to_file (prove2 (parse_expr line)) "completeness_output.txt";;