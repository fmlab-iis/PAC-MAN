
open Arg

type action = 
    Skip
  | Bad
  | Maybe
  | Trace of bool array

let action = ref Skip
let entry = ref "main"
let isPDA = ref false
let exclude_fpc_removal = ref false
let exclude_inline = ref false
let check_error_reachable = ref false
let check_error_reachable_stderr = ref false
                 
let args =
  [
    ("-c", Int (fun n -> Pac.compress := n), "Set the compression level (0, 1, or 2) applied to the generated trace.");
    ("-nfa", Unit (fun () -> isPDA := false), "Generate NFA.");
    ("-pda", Unit (fun () -> isPDA := true), "Generate PDA.");
    ("-f", String (fun str -> entry := str), "Specify the entry function."); 
    ("-F", Unit (fun () -> action := Maybe), "Produce an approximate automaton for feasible decisions."); 
    ("-b", Unit (fun () -> action := Bad), "Produce the bad automaton for the entry function.");
    ("-t", String (fun str -> action := Trace (Pac.decisions_from_string str)), "Produce a trace based on branch decisions starting from the entry function.");
    ("-ta", Unit (fun () -> Pac.typesig_from_actuals := true), "Determine the type of a function pointer from its invocations instead of\n      from its declaration.");
    ("-r", Unit (fun () -> check_error_reachable := true), "Check if some verifier error is reachable in a given trace and return only\n     the result. The result is empty if the trace is empty.");
    ("-re", Unit (fun () -> check_error_reachable_stderr := true), "Output the result of reachability checking of verifier errors to stderr.\n      The trace will be output to stdout with this option.");
    ("-s", Unit (fun () -> Cil.useLogicalOperators := true), "Reduce the number of branch conditions.");
    ("-xi", Unit (fun () -> exclude_inline := true), "Exclude function inlining in pre-process.");
    ("-xfpcr", Unit (fun () -> exclude_fpc_removal := true), "Exclude removal of function calls via function pointers.")
  ]

let usage = "pac [-F | -b | -t DECISIONS | -t FILENAME] [-f FUNCTION] [options ...] FILE"

let get_file filename =
  let fn = 
    if Sys.file_exists filename then
      filename
    else
      let fn = Filename.temp_file "cilengine" "cfile" in
      let ch = open_out fn in
      let _ = output_string ch filename in
      let _ = close_out ch in
      fn in
  Frontc.parse fn ()

let main filename =
  let file = get_file filename in
  (* Start of preprocessing *)
  let _ = if not !exclude_fpc_removal then Pac.remove_function_pointers file in
  let _ = if not !exclude_inline then Pac.inline file in
  let _ = Pac.add_verifier_assume_decl file in
  let _ = Pac.replace_assert file in
  let _ = Cil.iterGlobals file
                          (fun g ->
                           match g with
                             Cil.GFun (fd, _) ->
                             let _ = Oneret.oneret fd in
                             let _ = Cil.prepareCFG fd in
                             let _ = Cil.computeCFGInfo fd true in
                             ()
                           | _ -> ()) in
  (* End of preprocessing *)
  match !action with
    Skip -> Pac.pFile file
  | Bad -> Bad.go !isPDA file !entry
  | Maybe -> Maybe.go !isPDA file !entry
  | Trace decisions ->
    if !check_error_reachable then
      begin
        let trace = Pac.trace file ~fname:!entry decisions in
        if Pac.is_trace_empty trace then
          ()
        else
          let res = Pac.error_reachable file ~fname:!entry decisions in
          if !check_error_reachable_stderr then
            (prerr_endline (string_of_bool res); Pac.pTrace trace)
          else
            print_endline (string_of_bool res)
      end
    else
      let trace = Pac.trace file ~fname:!entry decisions in
      Pac.pTrace trace
    
let _ = 
  let _ = Errormsg.logChannel := stderr in
  parse args main usage
