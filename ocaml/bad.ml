
(* compute bad automata of the named function description *)

let go isPDA file fname =
  let fundecs = BadUtil.get_fundecs file.Cil.globals in

  let fdec_hash = BadUtil.build_fundec_hash fundecs in
  let fdec = 
    try Hashtbl.find fdec_hash fname 
    with Not_found -> 
      failwith ("The function " ^ fname ^ " is not found.") in
  let initials = 
    (* the first statement in the top fdec is initial *) 
    let first = List.hd fdec.Cil.sbody.Cil.bstmts in
    [ first.Cil.sid ] in
  let fdecs, max_stmtid = BadUtil.compute_invoked_fundecs fdec_hash fdec in
  (* print function names and max stmtid *)
  (*
  let _ =
    List.iter (fun fdec -> print_endline fdec.Cil.svar.Cil.vname) fdecs;
    print_int max_stmtid; print_newline () in
   *)
  let transitions, finals, maxst = 
    if isPDA then
      BadPDA.compute_transitions fdec_hash fdecs max_stmtid
    else
      BadNFA.compute_transitions fdec_hash fdecs max_stmtid in
  (*
  let _ = List.iter BadUtil.print_transition transitions in
   *)
  let _ = 
    if isPDA then
      BadPDA.print_pda maxst initials finals transitions
    else
      BadNFA.print_libalf maxst initials finals transitions in
  ()
                                  
