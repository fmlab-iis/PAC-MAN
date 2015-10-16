(* convert instructions to transitions *)

let instrs_to_transitions fdec_hash finals returns instrs sid succ_sids stmtid =
  let instrs_to_transitions_h (has_error, rev_ret, fin, rets, prev_stid, stmtid)
                              instr =
    if has_error then (has_error, rev_ret, fin, rets, prev_stid, stmtid)
    else
      match instr with
      | Cil.Set (_, exp, _) 
      | Cil.Call (_, exp, _, _) ->
         (match BadUtil.collect_call_in_exp exp with
          | Some fname -> 
             if fname = BadT.cSVCOMPERROR then
               let stmtid' = succ stmtid in
               let epsilon = (prev_stid, BadT.Epsilon, stmtid') in
               let fin' = stmtid'::fin in
               (true, epsilon::rev_ret, fin', rets, stmtid', stmtid')
             else if Hashtbl.mem fdec_hash fname then
               let fdec = Hashtbl.find fdec_hash fname in
               let stmtid' = succ stmtid in
               let f_stid, _ = BadUtil.first_and_last_stids fdec in
               let push_first = (prev_stid, BadT.Push stmtid', f_stid) in
               has_error, (push_first::rev_ret), fin, (stmtid'::rets), 
               stmtid', stmtid'
             else
               has_error, rev_ret, fin, rets, prev_stid, stmtid
          | None -> has_error, rev_ret, fin, rets, prev_stid, stmtid)
      | _ -> has_error, rev_ret, fin, rets, prev_stid, stmtid in
  let has_error, rev_ret, finals', returns', prev_stid', stmtid' =
    List.fold_left instrs_to_transitions_h 
                   (false, [], finals, returns, sid, stmtid) instrs in
  let last_transitions = 
    if has_error then []
    else BadUtil.epsilon_transitions prev_stid' succ_sids in
  (List.rev_append rev_ret last_transitions), finals', returns', stmtid'

(* convert a statement to transitions to its successors *)
(* for branching statements, exactly two successors are expected *)
(* the first successor is else-branch, the other is then-branch *)

let stmt_to_transitions fdec_hash finals returns stmt stmtid =
  let print_bstmt_ids block =
    let _ = print_string "< " in
    let _ = List.iter (fun stmt -> print_int stmt.Cil.sid; print_string " ")
                      block.Cil.bstmts in
    let _ = print_endline ">" in
    () in
  let sid = stmt.Cil.sid in
  let succ_sids = BadUtil.stmts_to_sids stmt.Cil.succs in
  (*
  let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt stmt in
   *)
  match stmt.Cil.skind with
  | Cil.If _ ->
     (*
     let _ = print_string "If (" in
     let _ = print_int sid; print_endline ")" in
     let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt stmt in
      *)
     (match succ_sids with
      | [ else_sid; then_sid ] ->
         [ (sid, BadT.False, else_sid); (sid, BadT.True, then_sid) ], 
         finals, returns, stmtid
      | _ -> assert false)
  | Cil.Instr instrs ->
     (*
     let _ = print_string "Instr (" in
     let _ = print_int sid; print_endline ")" in
     let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt stmt in
      *)
     instrs_to_transitions fdec_hash finals returns instrs sid succ_sids stmtid
  | Cil.Block block ->
     (*
     let _ = print_string "Block (" in
     let _ = print_int sid in
     let _ = print_string " [ " in
     let _ = List.iter (fun succid -> print_int succid; print_string " ")
                       succ_sids; print_endline "])" in
     let _ = print_bstmt_ids block in
     let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt stmt in
      *)
     (BadUtil.epsilon_transitions sid succ_sids), finals, returns, stmtid
  | Cil.Switch _ -> failwith "switch statement is not supported."
  | Cil.Loop (block, _, some_st_stmt, some_br_stmt) -> 
     (*
     let st_stmt, br_stmt = match some_st_stmt, some_br_stmt with
       | Some st, Some br -> st, br | _ -> assert false in
     let _ = print_string "Loop (" in
     let _ = print_int sid; print_string " { " in
     let _ = print_int st_stmt.Cil.sid; print_string " " in
     let _ = print_int br_stmt.Cil.sid; print_string " }" in
     let _ = print_string " [ " in
     let _ = List.iter (fun succid -> print_int succid; print_string " ")
                       succ_sids; print_endline "])" in
     let _ = print_bstmt_ids block in
      *)
     (BadUtil.epsilon_transitions sid succ_sids), finals, returns, stmtid
  | _ -> 
     (BadUtil.epsilon_transitions sid succ_sids), finals, returns, stmtid

(* obtain all transitions of a function description *)

let fundec_to_transitions fdec_hash finals returns fdec stmtid =
  let fundec_to_transitions_h (rev_ret, fins, rets, stid) stmt =
    let trs, fins', rets', stid' = 
      stmt_to_transitions fdec_hash fins rets stmt stid in
    trs::rev_ret, fins', rets', stid' in
  let rev_transitionss, finals', returns', stmtid' =
    List.fold_left fundec_to_transitions_h ([], finals, returns, stmtid) 
                   fdec.Cil.sallstmts in
  (List.flatten (List.rev rev_transitionss), finals', returns', stmtid')

(* add pop transitions for every function ends *)

let epsilon_pop_transitions returns fundecs stmtid =
  let stmtid' = succ stmtid in
  let last_epsilons = List.fold_left (fun ret fdec ->
    let _, last_stid = BadUtil.first_and_last_stids fdec in
    (last_stid, BadT.Epsilon, stmtid')::ret) [] fundecs in
  let pop_transitions = List.rev_map (fun return ->
    (stmtid', BadT.Pop return, return)) returns in
  last_epsilons, pop_transitions, stmtid'

(* connect to a fresh final state and pop all stack symbol *)

let epsilon_final_transitions finals returns stmtid =
  let stmtid' = succ stmtid in
  let final_epsilons = List.rev_map (fun final ->
    (final, BadT.Epsilon, stmtid')) finals in
  let final_pops = List.fold_left (fun ret return ->
    (stmtid', BadT.Pop return, stmtid')::ret) [] (-1::returns) in
  final_epsilons, final_pops, [stmtid'], stmtid'

(* compute transitions of function descriptions with internal  *)
(* states from 1 + stmt_id                                         *)

let compute_transitions fdec_hash fundecs stmtid =
  let compute_transitions_h (res, fin, ret, stid) fdec =
    let transitions, fin', ret', stid' = 
      fundec_to_transitions fdec_hash fin ret fdec stid in
    transitions::res, fin', ret', stid' in
  let transitionss, finals, returns, stmtid' = 
    List.fold_left compute_transitions_h ([], [], [], stmtid) fundecs in
  let last_epsilons, pop_transitions, stmtid'' =
    epsilon_pop_transitions returns fundecs stmtid' in
  let final_epsilons, final_pops, finals', last_stmtid =
    epsilon_final_transitions finals returns stmtid'' in
  List.flatten 
    (final_epsilons::final_pops::last_epsilons::pop_transitions::transitionss), 
  finals', last_stmtid

(* print AMoR format *)
(* Note that the state id starts from 0 upto highestid in AMoR format *)

let print_pda max_stmtid initial_states final_states transitions =
  let _ = print_endline "/*Initial State*/" in
  let _ = assert (List.length initial_states = 1) in
  let _ = print_int (List.hd initial_states); print_newline () in
  let _ = print_endline "/*Final States*/" in
  let _ = match final_states with
          | [] -> print_newline ()
          | [f] -> print_int f; print_newline ()
          | f::fs -> print_int f; 
                     List.iter (fun ff -> print_string " "; print_int ff) fs;
                     print_newline () in
  let _ = print_endline "/*Transitions*/" in
  let print_pda_h (f, l, t) =
    (print_int f; print_string " "; print_int t;
     match l with
     | BadT.Epsilon -> print_endline " I 0"
     | BadT.True -> print_endline " I 1"
     | BadT.False -> print_endline " I 2"
     | BadT.Push s -> print_string " U "; print_int s; print_newline ()
     | BadT.Pop s -> print_string " O "; print_int s; print_newline ()) in
  let _ = List.iter print_pda_h transitions in
  ()
