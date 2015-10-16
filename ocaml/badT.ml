
(* symbols of bad automata *)

type symbol_t = False | True | Epsilon | Push of int | Pop of int

let cSVCOMPERROR = "__VERIFIER_error"
