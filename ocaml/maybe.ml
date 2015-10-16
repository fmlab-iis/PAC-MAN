
(* compute over-approximation decision automata 
   of the named function description *)

let compute_nfa_transitions fdec_hash fundecs stmtid =
  let compute_transitions_h (res, fin, stid) fdec =
    let transitions, fin', stid' = 
      BadNFA.fundec_to_transitions fdec_hash fin fdec stid in
    transitions::res, fin', stid' in
  let transitionss, finals, stmtid' = 
    List.fold_left compute_transitions_h ([], [], stmtid) fundecs in
  let final_epsilons, _, finals', last_stmtid =
    BadNFA.epsilon_final_transitions finals stmtid' in
  List.flatten (final_epsilons::transitionss), finals', [], last_stmtid

let compute_pda_transitions fdec_hash fundecs stmtid =
  let compute_transitions_h (res, fin, ret, stid) fdec =
    let transitions, fin', ret', stid' = 
      BadPDA.fundec_to_transitions fdec_hash fin ret fdec stid in
    transitions::res, fin', ret', stid' in
  let transitionss, finals, returns, stmtid' = 
    List.fold_left compute_transitions_h ([], [], [], stmtid) fundecs in
  let last_epsilons, pop_transitions, stmtid'' =
    BadPDA.epsilon_pop_transitions returns fundecs stmtid' in
  let final_epsilons, _, finals', last_stmtid =
    BadPDA.epsilon_final_transitions finals returns stmtid'' in
  List.flatten (final_epsilons::last_epsilons::pop_transitions::transitionss),
  finals', returns, last_stmtid

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
  let transitions, _, returns, maxst = 
    if isPDA then
      compute_pda_transitions fdec_hash fdecs max_stmtid
    else
      compute_nfa_transitions fdec_hash fdecs max_stmtid in
  let finals = 
    let rec finals_h res i = 
      if i < 0 then res else finals_h (i::res) (pred i) in
    finals_h [] maxst in
  let _ = 
    if isPDA then
      let pop_returns f returns =
        List.rev_map (fun r -> (f, BadT.Pop r, f)) returns in
      let final_pops = List.fold_left (fun ret f ->
        List.rev_append ((f, BadT.Pop (-1), f)::(pop_returns f returns)) ret)
                                      [] finals in
      BadPDA.print_pda maxst initials finals 
                       (List.rev_append final_pops transitions)
    else
      BadNFA.print_libalf maxst initials finals transitions in
  ()
                                  
