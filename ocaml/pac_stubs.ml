
let enable_fpc_removal = ref true

let enable_inline = ref true

let set_use_logical_operators b = Cil.useLogicalOperators := b

let set_fpc_removal b = enable_fpc_removal := b

let set_inline b = enable_inline := b

let stubs file =
  let _ = if !enable_fpc_removal then Pac.remove_function_pointers file in
  let _ = if !enable_inline then Pac.inline file in
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
object
  method get_trace entry decisions = Pac.trace file ~fname:entry (Pac.decisions_from_string decisions)

  method save_trace trace output = Pac.save_trace trace output

  method is_error_reachable entry decisions = Pac.error_reachable file ~fname:entry (Pac.decisions_from_string decisions)

  method get_nfa entry = Bad.go false file entry

  method get_pda entry = Bad.go true file entry

  method get_nfa_approximation entry = Maybe.go false file entry

  method get_pda_approximation entry = Maybe.go true file entry
end

let hello () = print_endline "Hello World"
  
let create_stubs filename = stubs (Frontc.parse filename ())

let get_trace stubs entry decisions = stubs#get_trace entry decisions

let save_trace stubs trace output = stubs#save_trace trace output

let is_error_reachable stubs entry decisions = stubs#is_error_reachable entry decisions

let get_nfa stubs entry = stubs#get_nfa entry

let get_pda stubs entry = stubs#get_pda entry

let get_nfa_approximation stubs entry = stubs#get_nfa_approximation entry

let get_pda_approximation stubs entry = stubs#get_pda_approximation entry

let _ = Callback.register "pac_hello" hello
  
let _ = Callback.register "pac_set_use_logical_operators" set_use_logical_operators

let _ = Callback.register "pac_set_fpc_removal" set_fpc_removal

let _ = Callback.register "pac_set_inline" set_inline

let _ = Callback.register "pac_create_stubs" create_stubs

let _ = Callback.register "pac_get_trace" get_trace

let _ = Callback.register "pac_get_nfa" get_nfa

let _ = Callback.register "pac_get_pda" get_pda

let _ = Callback.register "pac_get_nfa_approximation" get_nfa_approximation

let _ = Callback.register "pac_get_pda_approximation" get_pda_approximation
