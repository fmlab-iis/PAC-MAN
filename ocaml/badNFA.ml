(* convert instructions to transitions *)

let instrs_to_transitions fdec_hash finals instrs sid succ_sids stmtid =
  let instrs_to_transitions_h (has_error, rev_ret, fin, prev_stid, stmtid) 
                              instr =
    if has_error then (has_error, rev_ret, fin, prev_stid, stmtid)
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
               (true, epsilon::rev_ret, fin', stmtid', stmtid')
             else if Hashtbl.mem fdec_hash fname then
               let fdec = Hashtbl.find fdec_hash fname in
               let stmtid' = succ stmtid in
               let f_stid, l_stid = BadUtil.first_and_last_stids fdec in
               let epsilon_first = 
                 List.hd (BadUtil.epsilon_transitions prev_stid [ f_stid ]) in
               let epsilon_last = 
                 List.hd (BadUtil.epsilon_transitions l_stid [ stmtid' ]) in
               has_error, (epsilon_last::epsilon_first::rev_ret), fin, 
               stmtid', stmtid'
             else
               has_error, rev_ret, fin, prev_stid, stmtid
          | None -> has_error, rev_ret, fin, prev_stid, stmtid)
      | _ -> has_error, rev_ret, fin, prev_stid, stmtid in
  let has_error, rev_ret, finals', prev_stid', stmtid' =
    List.fold_left instrs_to_transitions_h (false, [], finals, sid, stmtid) 
                   instrs in
  let last_transitions = 
    if has_error then [] 
    else BadUtil.epsilon_transitions prev_stid' succ_sids in
  (List.rev_append rev_ret last_transitions), finals', stmtid'

(* convert a statement to transitions to its successors *)
(* for branching statements, exactly two successors are expected *)
(* the first successor is else-branch, the other is then-branch *)

let stmt_to_transitions fdec_hash finals stmt stmtid =
  let sid = stmt.Cil.sid in
  let succ_sids = BadUtil.stmts_to_sids stmt.Cil.succs in
  (*
  let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt stmt in
   *)
  match stmt.Cil.skind with
  | Cil.If _ ->
     (match succ_sids with
      | [ else_sid; then_sid ] ->
         [ (sid, BadT.False, else_sid); (sid, BadT.True, then_sid) ], 
         finals, stmtid
      | _ -> assert false)
  | Cil.Instr instrs ->
     instrs_to_transitions fdec_hash finals instrs sid succ_sids stmtid
  | _ -> (BadUtil.epsilon_transitions sid succ_sids), finals, stmtid

(* obtain all transitions of a function description *)

let fundec_to_transitions fdec_hash finals fdec stmtid =
  let fundec_to_transitions_h (rev_ret, fins, stid) stmt =
    let trs, fins', stid' = stmt_to_transitions fdec_hash fins stmt stid in
    trs::rev_ret, fins', stid' in
  let rev_transitionss, finals', stmtid' =
    List.fold_left fundec_to_transitions_h ([], finals, stmtid) 
                   fdec.Cil.sallstmts in
  (List.flatten (List.rev rev_transitionss), finals', stmtid')

(* connect to a fresh final state and read remaining symbols *)

let epsilon_final_transitions finals stmtid =
  let stmtid' = succ stmtid in
  let final_epsilons = List.rev_map (fun final ->
    (final, BadT.Epsilon, stmtid')) finals in
  let final_reads = 
    [(stmtid', BadT.False, stmtid'); (stmtid', BadT.True, stmtid')] in
  final_epsilons, final_reads, [stmtid'], stmtid'

(* compute transitions of function descriptions with internal  *)
(* states from 1 + stmt_id                                         *)

let compute_transitions fdec_hash fundecs stmtid =
  let compute_transitions_h (res, fin, stid) fdec =
    let transitions, fin', stid' = 
      fundec_to_transitions fdec_hash fin fdec stid in
    transitions::res, fin', stid' in
  let transitionss, finals, stmtid' = 
    List.fold_left compute_transitions_h ([], [], stmtid) fundecs in
  let final_epsilons, final_reads, finals', last_stmtid =
    epsilon_final_transitions finals stmtid' in
  List.flatten (final_epsilons::final_reads::transitionss), 
  finals', last_stmtid

(* print AMoR format *)
(* Note that the state id starts from 0 upto highestid in AMoR format *)

let print_amore max_stmtid initial_states final_states transitions =
  let cFINAL = 1 in
  let cINITIAL = 2 in
  let cSIZEOFBYTE = 8 in
  let num_states = succ max_stmtid in
  let cLASTDELTA = num_states / cSIZEOFBYTE in
  let trans = Array.init 3 (fun _ -> 
    Array.init num_states (fun _ -> Array.create (succ cLASTDELTA) 0)) in
  (* from amor/nfa.h
     D[L][F][(T)/SIZEOFBYTE] |= 0x1 << ((T)%SIZEOFBYTE)
   *)
  let connect (s, l, t) =
    let l_idx = match l with 
      | BadT.Epsilon -> 0 | BadT.False -> 1 | BadT.True -> 2 | _ -> assert false 
    in
    let s_idx = s in
    let t_idx = t / cSIZEOFBYTE in
    let t_val = 1 lsl (t mod cSIZEOFBYTE) in
    trans.(l_idx).(s_idx).(t_idx) <- 
      trans.(l_idx).(s_idx).(t_idx) lor t_val in
  let _ = List.iter connect transitions in
  let initfin = Array.create num_states 0 in
  let _ = List.iter (fun i -> initfin.(i) <- cINITIAL) initial_states in
  let _ = List.iter (fun f -> initfin.(f) <- initfin.(f) lor cFINAL) 
                    final_states in
  let _ = print_endline "AMORE Language Save File VERSION 1.0" in
  let _ = print_endline "NAME " in
  let _ = print_endline "Bad NFA" in
  let _ = print_endline "A bad nfa in AMoRE NFA format" in
  let _ = print_endline "ALPHABET " in
  let _ = print_endline "2" in
  let _ = print_endline "INPUT" in
  let _ = print_endline "12" in (* could be 13 *)
  let _ = print_endline "NFA: #STATES" in
  let _ = print_int max_stmtid;
          print_newline () in
  let _ = print_endline "NFA: ALPHABET" in
  let _ = print_endline "2" in
  let _ = print_endline "NFA: EPSILON, MINIMAL" in
  let _ = print_endline "T"; print_endline "F" in
  let _ = print_endline "NFA: FINAL STATES" in
  let _ = Array.iter (fun v -> print_int v; print_newline ()) initfin in
  let _ = print_endline "NFA: TRANSFORMATION" in
  let _ = for k = 0 to cLASTDELTA do
            for i = 0 to 2 do
              for j = 0 to max_stmtid do
                (print_int trans.(i).(j).(k); print_newline ())
              done
            done
          done in
  let _ = print_endline "NFA: END" in
  let _ = print_endline "BOOLE FLAGS: (BREX,BDFA,BNFA,BENFA,BMON) " in
  let _ = print_endline "REGULAR EXPRESSION " in
  let _ = print_endline "NONE" in
  let _ = print_endline "DETERMINISTIC FINITE AUTOMATON " in
  let _ = print_endline "NONE" in
  let _ = print_endline "NONDETERMINISTIC FINITE AUTOMATON " in
  let _ = print_endline "NONE" in
  let _ = print_endline "MONOID " in
  let _ = print_endline "NONE" in
  let _ = print_endline "STARFREE EXPRESSION " in
  let _ = print_endline "NONE" in
  let _ = print_endline "EMPTY,FULL,FOLU,SF,LOC.TESTABLE,DEFINITE,REV.D.,GEN.D.,DOTDEPTH,NILPOTENT" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "Unknown" in
  let _ = print_endline "0" in (* what is localdegree? *)
  ()

(* print libalf format *)

let print_libalf max_stmtid initial_states final_states transitions =
  let print_states states =
    match states with
    | [ st ] -> Printf.printf "\t%i;\n" st
    | st::sts -> 
       (Printf.printf "\t%i" st;
        List.iter (fun s -> Printf.printf ", %i" s) sts;
        Printf.printf ";\n")
    | [] -> Printf.printf "\t;\n" in
  let _ = Printf.printf "[general]\n" in
  let _ = Printf.printf "\tis dfa = false;\n" in
  let _ = Printf.printf "\talphabet size = 2;\n" in
  let _ = Printf.printf "\tnumber of states = %i;\n" (succ max_stmtid) in
  let _ = Printf.printf "[initial states]\n" in
  let _ = print_states initial_states in
  let _ = Printf.printf "[final states]\n" in
  let _ = print_states final_states in
  let _ = Printf.printf "[transitions]\n" in
  let _ = List.iter (fun (f, l, t) ->
    Printf.printf "\t%i, %2i, %i;\n"
                  f 
                  (match l with 
                   | BadT.Epsilon -> -1 | BadT.True -> 1 | BadT.False -> 0
                   | _ -> assert false)
                  t) 
                    transitions in
  (* additional newline? *)
  let _ = Printf.printf "\n" in
  ()

  
