
(* utility functions for bad automata construction *)

(* retrieve function descriptions *)

let get_fundecs globals =
  let fundec_h res global =
    match global with
    | Cil.GFun (fdec, _) -> fdec::res
    | _ -> res in
  List.rev (List.fold_left fundec_h [] globals)

(* check if a statment is a branching statement *)

let stmt_is_branch stmt =
  match stmt.Cil.skind with
  | Cil.If _ -> true
  | _ -> false

(* convert statements to statement ids *)

let stmts_to_sids stmts =
  let stmts_to_sids_h stmt = stmt.Cil.sid in
  List.rev (List.rev_map stmts_to_sids_h stmts)

(* find the first and last statement ids in a function description *)

let first_and_last_stids fdec =
  (* assume the first stmt is the first one in the sbody *)
  let first_stmt = List.hd fdec.Cil.sbody.Cil.bstmts in
  (* let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt first_stmt in *)
  (*
  let _ = assert (first_stmt.Cil.preds = []) in
   *)
  (* assume the last stmt is the last one in the sbody *)
  let last_stmt = List.hd (List.rev fdec.Cil.sbody.Cil.bstmts) in
  (* let _ = Pretty.printf "STMT:%a\n" Cil.d_stmt last_stmt in *)
  let _ = assert (last_stmt.Cil.succs = []) in
  (first_stmt.Cil.sid, last_stmt.Cil.sid)

(* collect the function description called in an expression *)

let rec collect_call_in_exp exp =
  (*
  let _ = Pretty.printf "EXP:%a\n" Cil.d_exp exp in
   *)
  match exp with
  | Cil.Lval (host, _) ->
     (match host with
      | Cil.Var vinfo -> Some vinfo.Cil.vname
      | Cil.Mem e -> collect_call_in_exp e)
  | _ -> None

(* make epsilon transitions from sid to each of succ_sids *)

let epsilon_transitions sid succ_sids =
  let epsilon_transitions_h succ_sid = (sid, BadT.Epsilon, succ_sid) in
  List.rev (List.rev_map epsilon_transitions_h succ_sids)

(* collect all function descriptions called in instr list *)

let collect_call_in_instrs fdec_hash instrs rest =
  let collect_call_in_instrs_h ret instr =
    match instr with
    | Cil.Call (_, exp, _, _) 
    | Cil.Set (_, exp, _) ->
       (match collect_call_in_exp exp with
        | None -> ret
        | Some fname ->
           if Hashtbl.mem fdec_hash fname then
             (Hashtbl.find fdec_hash fname)::ret
           else
             ret)
    | _ -> ret in
  List.fold_left collect_call_in_instrs_h rest instrs

(* collect all function descriptions called in stmt list *)

let collect_callee_in_stmts fdec_hash stmts rest =
  let collect_callee_in_stmts_h ret stmt =
    match stmt.Cil.skind with
    | Cil.Instr instrs -> collect_call_in_instrs fdec_hash instrs ret
    | _ -> ret in
  List.fold_left collect_callee_in_stmts_h rest stmts

(* compute all function descriptions invoked by fundec *)

let compute_invoked_fundecs fdec_hash fundec =
  let visited = Hashtbl.create 91 in
  let rec compute_invoked_fundecs_h fdecs (ret, max_stmtid) =
    match fdecs with
    | fdec::rest ->
       if Hashtbl.mem visited fdec then
         compute_invoked_fundecs_h rest (ret, max_stmtid)
       else
         let _ = Hashtbl.add visited fdec () in
         let max_stmtid' = match fdec.Cil.smaxstmtid with
           | Some i -> max max_stmtid i | None -> assert false in
         let rest' = 
           collect_callee_in_stmts fdec_hash fdec.Cil.sallstmts rest in
         compute_invoked_fundecs_h rest' (fdec::ret, max_stmtid')
    | [] -> (ret, max_stmtid) in
  compute_invoked_fundecs_h [fundec] ([], 0)
                                       

(* construct a hashtable from function names to fundecs *)

let build_fundec_hash fdecs =
  let hash = Hashtbl.create 91 in
  let build_fundec_hash_h fdec =
    let fname = fdec.Cil.svar.Cil.vname in
    let _ = assert (not (Hashtbl.mem hash fname)) in
    Hashtbl.add hash fname fdec in
  let _ = List.iter build_fundec_hash_h fdecs in
  hash

 
(* print one transition *)

let print_transition (sid, symbol, succ_sid) =
  print_int sid; print_string " --";
  (match symbol with
   | BadT.False -> print_string "0"
   | BadT.True -> print_string "1"
   | BadT.Epsilon -> print_string "-"
   | BadT.Push i -> print_string "U("; print_int i; print_string ")"
   | BadT.Pop i -> print_string "O("; print_int i; print_string ")"
  );
  print_string "--> ";
  print_int succ_sid;
  print_newline ()

