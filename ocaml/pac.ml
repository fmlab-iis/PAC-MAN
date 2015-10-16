
open Cil

let compress = ref 0

let typesig_from_actuals = ref false
  
let main_name = "main"

let assert_name = "assert"
  
let verifier_error_name = "__VERIFIER_error"

let verifier_assert_name = "__VERIFIER_assert"
  
let verifier_assume_name = "__VERIFIER_assume"

(* ========== Auxiliary Functions ========== *)

let string_of_type typ = Pretty.sprint 80 (printType defaultCilPrinter () typ)

let string_of_instr instr = Pretty.sprint 80 (printInstr defaultCilPrinter () instr)

let string_of_lval lv = Pretty.sprint 80 (printLval defaultCilPrinter () lv)

let string_of_exp exp = Pretty.sprint 80 (printExp defaultCilPrinter () exp)

let pGlobal global = print_endline (Pretty.sprint 80 (printGlobal defaultCilPrinter () global))

let pExp exp = print_endline (Pretty.sprint 80 (printExp defaultCilPrinter () exp))
  
let pStmt stmt = print_endline (Pretty.sprint 80 (printStmt defaultCilPrinter () stmt))

let pFile file = dumpFile defaultCilPrinter stdout "" file

let save_file file output =
  let outch = open_out output in
  let _ = dumpFile defaultCilPrinter outch "" file in
  let _ = close_out outch in
  ()

(** Returns an instruction that assumes a specified expression which 
    encodes a formula supported by the underlying verifier. *)
let mkAssume exp =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true verifier_assume_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [exp], locUnknown) in
  instr

(** Returns an assert instruction. *)
let mkAssert exp =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true verifier_assert_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [exp], locUnknown) in
  instr
    
(** Returns a statement if (e) __VERIFIER_error(). *)
let mkErrorInstr () =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true verifier_error_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  Call (None, lval, [], locUnknown)
    
let mkErrorStmt exp = mkStmt (If (exp, mkBlock ([mkStmt (Instr [mkErrorInstr ()])]), mkBlock [], locUnknown))

(** Return all the function declarations in a file. *)
let get_fundec_map file =
  let m = Hashtbl.create 10 in
  let _ = List.iter (
    fun global ->
      match global with
        GFun (fd, _) -> Hashtbl.add m fd.svar.vname fd
      | _ -> ()
  ) file.globals in
  m

(** Insert functions for verifiers. *)
let insert_verifier_functions file =
  let mkassert () =
    let fundec = emptyFunction verifier_assert_name in
    let cond = Lval (Var (makeFormalVar fundec "expression" intType), NoOffset) in
    let goto = {
      labels = [Label ("ERROR", locUnknown, false)];
      skind = Block {battrs = []; bstmts = []};
      sid = 1;
      succs = [];
      preds = []
    } in
    let _ = goto.skind <- Goto (ref goto, locUnknown) in
    let return = {
      labels = [];
      skind = Return (None, locUnknown);
      sid = 2;
      succs = [];
      preds = []
    } in
    let ifs = {
      labels = [];
      skind = If (
        UnOp (LNot, cond, intType),
        {battrs = []; bstmts = [goto]},
        {battrs = []; bstmts = []},
        locUnknown);
      sid = 0;
      succs = [];
      preds = []
    } in
    let _ = fundec.sbody.bstmts <- [ifs; return] in
    fundec in
  let mkassume () =
    let fundec = emptyFunction verifier_assume_name in
    let cond = Lval (Var (makeFormalVar fundec "expression" intType), NoOffset) in
    let goto = {
      labels = [Label ("LOOP", locUnknown, false)];
      skind = Block {battrs = []; bstmts = []};
      sid = 1;
      succs = [];
      preds = []
    } in
    let _ = goto.skind <- Goto (ref goto, locUnknown) in
    let return = {
      labels = [];
      skind = Return (None, locUnknown);
      sid = 2;
      succs = [];
      preds = []
    } in
    let ifs = {
      labels = [];
      skind = If (
        UnOp (LNot, cond, intType),
        {battrs = []; bstmts = [goto]},
        {battrs = []; bstmts = []},
        locUnknown);
      sid = 0;
      succs = [];
      preds = []
    } in
    let _ = fundec.sbody.bstmts <- [ifs; return] in
    fundec in
  let ast = mkassert () in
  let asm = mkassume () in
  file.globals <- (GFun (ast, locUnknown))::(GFun (asm, locUnknown))::file.globals

(** 
    Replaces assert(e) by if (!(e)) __VERIFIER_error. 
    This function will cause problems when there is a switch statement.
    CIL will not be able to find the reference statement of a goto statement.
*)
let replace_assert file =
  let h = Hashtbl.create 10 in
  let nonempty stmt =
    match stmt.skind with
      Instr [] -> false
    | _ -> true in
  let has_assert instrs =
    List.exists (fun instr ->
      match instr with
        Call (_, Lval (Var fv, NoOffset), [exp], _) when fv.vname = assert_name -> true
      | _ -> false) instrs in
  let replace_instr instr =
    match instr with
      Call (_, Lval (Var fv, NoOffset), [exp], _) when fv.vname = assert_name ->
        mkErrorStmt (UnOp (LNot, exp, typeOf exp))
    | _ -> mkStmt (Instr [instr]) in
  let replace_instrs instrs = compactStmts (List.filter nonempty (List.map replace_instr instrs)) in
  let replace_visitor =
object(self)
  inherit nopCilVisitor as super
  method vstmt stmt =
    match stmt.skind with
      Instr instrs when has_assert instrs ->
        let repl =
          match replace_instrs instrs with
            stmt::[] -> stmt
          | _ as stmts -> mkStmt (Block (mkBlock stmts)) in
        let _ = repl.labels <- stmt.labels in
        let _ = Hashtbl.add h stmt repl in
        ChangeTo repl
    | _ -> DoChildren
end in
  (* Fix goto references. *)
  let fix_visitor =
object(self)
  inherit nopCilVisitor as super
  method vstmt stmt =
    match stmt.skind with
      Goto (sref, loc) ->
        if Hashtbl.mem h !sref then
          let repl = mkStmt (Goto (ref (Hashtbl.find h !sref), loc)) in
          let _ = repl.labels <- stmt.labels in
          ChangeTo repl
        else
          DoChildren
    | _ -> DoChildren
end in
  let _ = visitCilFileSameGlobals (replace_visitor :> cilVisitor) file in
  let _ = visitCilFileSameGlobals (fix_visitor :> cilVisitor) file in
  ()

class myVisitor =
object(self)
  inherit nopCilVisitor

  val gmap = Hashtbl.create 100

  (** The current function to be visited *)
  val mutable cfun = None

  (** Returns the current function in visiting. *)
  method cfun =
    match cfun with 
      Some f -> f
    | None -> failwith ("Unable to determine the current function during visiting the CIL file.")

  method cgvars = gmap

  (** Remembers all the global variables seen so far. *)
  method vglob glob =
    let _ = 
      match glob with
        GVar (v, _, _) -> Hashtbl.add gmap v.vname v
      | _ -> () in
    DoChildren

  method vfunc fundec : fundec visitAction =
    let _ = cfun <- Some fundec in
    DoChildren
end

(** 
    This class provides a visitor that can collect additional information.
    Subclasses must call vglob and vfunc of this class.
*)

(** Inlines nonrecursive functions. *)
let inline file =
  let is_user_function =
    let fundecs = List.flatten (List.map (
      fun g ->
        match g with
          GFun (fd, loc) -> [fd.svar.vname]
        | _ -> []) file.globals) in
    fun name ->
      (* is defined in the input C file *)
      List.mem name fundecs && 
        (* not built-in functions for the current compiler *)
        not (Hashtbl.mem Cil.builtinFunctions name) in
  (* A map from a caller to it callees. *)
  let fmap = Hashtbl.create 10 in
  (* A list of user functions. *)
  let fs = ref [] in
  let _ = visitCilFileSameGlobals (object
    inherit myVisitor as super
    method vinst instr =
      match instr with
        Call (res, Lval (Var fv, NoOffset), args, loc) ->
          let caller = (super#cfun).svar.vname in
          let _ =
            try
              let callees = Hashtbl.find fmap caller in
              if not (List.mem fv.vname callees) then
                Hashtbl.replace fmap caller (fv.vname::callees)
            with Not_found ->
              Hashtbl.add fmap caller [fv.vname] in
          DoChildren
      | _ -> DoChildren

    method vglob g =
      let _ = super#vglob g in
      let _ = 
        match g with
          GFun (fd, _) -> 
            if is_user_function fd.svar.vname then
              fs := fd.svar.vname::!fs
        | _ -> () in
      DoChildren
  end :> cilVisitor) file in
  let _ = Hash.saturate fmap in
  let isRecursive fn = try List.mem fn (Hashtbl.find fmap fn) with Not_found -> false in
  let toinline = List.filter (fun fn -> fn <> "main" && not (isRecursive fn)) !fs in
  (* Inline functions. *)
  let _ = Inliner.doit file toinline in
  (* Remove inlined functions. *)
  let _ = file.globals <- List.filter (
    fun g ->
      match g with
        GFun (fd, _) when List.mem fd.svar.vname toinline -> false
      | _ -> true
  ) file.globals in
  ()

let decisions_from_string str =
  let str = 
    if Sys.file_exists str then
      let inch = open_in str in
      let s =
        try
          input_line inch
        with End_of_file ->
          "" in
      let _ = close_in inch in
      s
    else
      str in
  let bs = Array.make (String.length str) true in
  let _ = String.iteri (
    fun i c ->
      if c = '0' then
        bs.(i) <- false
      else if c = '1' then
        bs.(i) <- true
      else
        failwith("Unrecognized decision '" ^ String.make 1 c ^
                    "'. Please specify the decisions as a string of 0's and 1's.")) str in
  bs



(* ========== Function Pointers Removal ========== *)

let rec string_of_typsig ts = Pretty.sprint 80 (d_typsig () ts)

let actual_typesig lv resopt args =
  let varargs =
    let typ = typeOfLval lv in
    match typ with
      TFun (_, _, b, _) -> b
    | _ -> failwith("Unexpected type of function " ^ string_of_lval lv ^ ": " ^ string_of_typsig (typeSig typ)) in
  let rt =
    match resopt with
      None -> TSBase (TVoid [])
    | Some r -> typeSig (typeOfLval r) in
  let ats = List.map (
    fun arg ->
      typeSig (typeOf arg)
  ) args in
  TSFun (rt, ats, varargs, [])

let build_typsig_map file =
  let map = Hashtbl.create (List.length file.globals) in
  let _ = List.iter (
    fun global ->
      match global with
        GFun (fd, _) ->
          let tsig = typeSig fd.svar.vtype in
          let fds = try Hashtbl.find map tsig with Not_found -> [] in
          Hashtbl.replace map tsig (fd::fds)
      | _ -> ()
  ) file.globals in
  map

let is_function_pointer_call instr =
  match instr with
    Call (_, Lval (Mem _, _), _, _) -> true
  | _ -> false
    
let remove_function_pointers file =
  let smap = build_typsig_map file in
  let mk_explicit_call resopt lv args fds loc =
    let rec helper fds : stmtkind =
      match fds with
        [] -> Block { battrs = []; bstmts = [] }
      | fd::fds ->
        let e = BinOp (Eq, Lval lv, AddrOf (var fd.svar), intType) in
        let call = Instr [Call (resopt, Lval (var fd.svar), args, loc)] in
        If (e, mkBlock [mkStmt call], mkBlock [mkStmt (helper fds)], loc) in
    helper fds in
  let remove_fpc instr =
    match instr with
      Call (resopt, Lval ((Mem _, _) as lv), args, loc) ->
        let tsig =
          match (if !typesig_from_actuals then
              actual_typesig lv resopt args
            else
              typeSig (typeOfLval lv)) with
            TSFun _ as t -> t
          | (_ as t) -> failwith ("The type signature of a function pointer " ^ string_of_lval lv ^
                                     " should be TSFun. The actual type signature is " ^
                                     string_of_typsig t) in
        let fds =
          try
            Hashtbl.find smap tsig
          with Not_found ->
            failwith("Failed to find matched functions for the function call " ^ string_of_instr instr)
        in
        mk_explicit_call resopt lv args fds loc
    | _ -> Instr [instr]
  in
  let visitor = 
object(self)
  inherit nopCilVisitor

  method vstmt stmt =
    match stmt.skind with
      Instr instrs ->
        if List.exists is_function_pointer_call instrs then
          let stmts = compactStmts (List.map mkStmt (List.map remove_fpc instrs)) in
          ChangeTo { labels = stmt.labels;
                     skind = Block { battrs = []; bstmts = stmts };
                     sid = stmt.sid;
                     succs = stmt.succs;
                     preds = stmt.preds }
        else
          DoChildren
    | _ -> DoChildren
end in
  Cil.visitCilFileSameGlobals visitor file




(* ========== Traces ========== *)

type trace = file option

let pTrace trace =
  match trace with
    None -> ()
  | Some file -> pFile file

let save_trace trace output =
  match trace with
    None ->
      let outch = open_out output in
      let _ = output_string outch "" in
      let _ = close_out outch in
      ()
  | Some file -> save_file file output

let is_trace_empty trace =
  match trace with
    None -> true
  | Some _ -> false
    
(* ========== Trace Extraction ========== *)
    
let add_suffix name index = name ^ "_call" ^ (string_of_int index)

let has_suffix name = 
  let regexp = Str.regexp "^\\([a-zA-Z0-9_]+\\)_call[0-9]+$" in
  Str.string_match regexp name 0
    
let remove_suffix name =
  let regexp = Str.regexp "^\\([a-zA-Z0-9_]+\\)_call[0-9]+$" in
  if Str.string_match regexp name 0 then
    Str.matched_group 1 name
  else
    name

(** Calculate all statements in a block. *)
let all_statements_block =
  let rec all_statements_stmt_rec all stmt =
    match stmt.skind with
      Instr _
    | Return _
    | Goto _
    | ComputedGoto _
    | Break _
    | Continue _ -> stmt::all
    | If (_, b1, b2, _) -> List.fold_left (fun res block -> all_statements_block_rec res block) (stmt::all) [b1; b2]
    | Switch (_, b, _, _)
    | Loop (b, _, _, _)
    | Block b -> all_statements_block_rec (stmt::all) b
    | TryFinally (b1, b2, _)
    | TryExcept (b1, _, b2, _) -> List.fold_left (fun res block -> all_statements_block_rec res block) (stmt::all) [b1; b2]
  and all_statements_block_rec all block = List.fold_left (fun res stmt -> all_statements_stmt_rec res stmt) all block.bstmts in
  all_statements_block_rec []
  
class helperClass file (decisions : bool array) =
  
  (* A map from a function name to its fundec. *)
  let fmap = Hashtbl.create (List.length file.globals) in
  
  (* A map from a function name to the index of the next call to the function. *)
  let cmap = Hashtbl.create (List.length file.globals) in

  (* The index of the next decision. *)
  let di = ref 0 in

  let finished = ref false in

  let insert_final_error = ref true in

  let continue_after_last_decision = ref false in
  
  let size = Array.length decisions in

  (* A flag indicating if some intermediate __VERIFIER_error is encountered. *)
  let error_encountered = ref false in

  let output = {
    fileName = file.fileName;
    globals = [];
    globinit = None;
    globinitcalled = false
  } in

  (* Initialize fmap, cmap, and the output file. *)
  let _ = List.iter
    (fun g ->
      match g with
        GFun (fd, _) ->
          let _ = Hashtbl.add fmap fd.svar.vname fd in
          let _ = Hashtbl.add cmap fd.svar.vname 0 in
          ()
      | _ when List.mem g output.globals ->
                (* Don't know why but sometimes the input file may have duplicate globals. *)
        ()
      | _ -> output.globals <- g::output.globals)
    file.globals in

  (* Returns the next index of the call to a function. *)
  let getNextCallIndex name =
    try
      let i = (Hashtbl.find cmap name) + 1 in
      let _ = Hashtbl.add cmap name i in
      i
    with Not_found ->
      let _ = Hashtbl.add cmap name 1 in
      1 in

  (**
     * A stack element is a tuple (pairs, stmts, cts, brs) where pairs is a list
     * of (start, dec_rev) with start marking the start of a loop in the output
     * and decs_rev used to record the decisions consumed in the loop, stmts is
     * a list of statement IDs of the loop in the input, acts is the set of
     * statement IDs of the continue labels in the input, and brs is the set of
     * statement IDs of the break statement in the input. We need a list of
     * pairs because the repetition of a loop body may has different patterns.
  *)
  let stack : ((stmt * bool list) list * int list * int list * int list) Stack.t ref = ref (Stack.create()) in

  (** Print the top element of the stack. *)
  let pstack () =
    try
      let (pairs, stmts, cts, brs) = Stack.top !stack in
      let _ = List.iter (
        fun (_, decs_rev) ->
          print_endline ("Decisions (reversed): " ^ String.concat "" (List.map (fun b -> if b then "1" else "0") decs_rev))
      ) pairs in
      let _ = print_endline ("Statements: " ^ String.concat " " (List.map string_of_int stmts)) in
      let _ = print_endline ("Continues: " ^ String.concat " " (List.map string_of_int cts)) in
      let _ = print_endline ("Breaks: " ^ String.concat " " (List.map string_of_int brs)) in
      ()
    with Stack.Empty ->
      () in

object(self)
  (** Returns the input file. *)
  method getInput = file

  (** Returns the output file. *)
  method getOutput = output

  method isInsertFinalError = !insert_final_error

  method setInsertFinalError b = insert_final_error := b

  method isContinueAfterLastDecision = !continue_after_last_decision

  method setContinueAfterLastDecision b = continue_after_last_decision := b
    
  (**
     * Returns the function definition of a specified name in the input file.
     * Throws Not_found if the function cannot be found.
  *)
  method getInputFundec name = Hashtbl.find fmap name

  (**
      * Returns a new function definition with a specified name in the output
      * file. The function is always copied from the function of the same name in
      * the input file except that the function body will be cleared. Not_found
      * will be thrown if the function of the name cannot be found in the input
      * file.
  *)
  method getOutputFundec name =
    let fd = copyFunction (self#getInputFundec name) name in
    let _ = fd.sbody.bstmts <- [] in
    let _ = output.globals <- (GFun (fd, locUnknown))::output.globals in
    fd

  (** 
      * Returns a new function definition same as in getOutputFundec except that
      * the function name will be renamed such that the returned function will be
      * unique. Not_found will be thrown if the function of the name cannot be
      * found in the input file.
  *)
  method getOutputFundecRenamed name =
    let fd = copyFunction (self#getInputFundec name) (add_suffix name (getNextCallIndex name)) in
    let _ = fd.sbody.bstmts <- [] in
    let _ = output.globals <- (GFun (fd, locUnknown))::output.globals in
    fd

  (** Returns true if there is a next branch decision. *)
  method hasNextDecision = !di < size

  (** Pops the next branch decision. *)
  method popNextDecision =
    let res = decisions.(!di) in
    let _ = incr di in
    res

  method setErrorEncountered = error_encountered := true

  method isErrorEncountered = !error_encountered

  method isFinished = !finished

  method setFinished b = finished := b

  (**
     * Find a best repeatable decision pattern in the top element of the stack,
     * repeat it, and return the number of repetition. If the top element
     * contains multiple decision patterns and some of them can repeat, only
     * the best one will be kept after this function. If none of them can
     * repeat, nothing changes.
  *)
  method advance =
    let dlen = Array.length decisions in
    (* Match a pattern once starting from a position in the decision vector. *)
    let rec match_once p pattern =
      if p >= dlen then
        (false, p)
      else
        match pattern with
          [] -> (true, p)
        | hd::tl when hd = decisions.(p) -> match_once (p + 1) tl
        | _ -> (false, p) in
    (* Match a pattern as more as possible starting from a position in the decision vector. *)
    let rec match_more n p pattern =
      match match_once p pattern with
        (true, p') when p' > p -> match_more (n + 1) p' pattern
      | _ -> (n, p) in
    (* Calculate the number of repetition of a pattern. The position of the next
       decision after the repetition of the pattern is also returned. *)
    let calc pattern = match_more 0 !di pattern in
    (* Find the best pattern for repetition. *)
    let best pairs =
      List.fold_left (
        fun res (start, decs_rev) ->
          let pattern = List.rev decs_rev in
          let (num, pos) = calc pattern in
          match res with
            Some (_, _, _, p, _) when p >= pos -> res
          | _ -> Some (start, decs_rev, num, pos, pattern)
      ) None pairs in
    try
      let (pairs, stmts, cts, brs) = Stack.top !stack in
      match best pairs with
        None -> 0
      | Some (start, decs_rev, num, pos, pattern) ->
        (* Insert the repeated patterns. *)
        let _ = Stack.pop !stack in
        let _ = Stack.push ([(start, decs_rev)], stmts, cts, brs) !stack in
        let _ = for i = 1 to num do self#pushLoopDecisions pattern done in
        (* Update the position of the next decision. *)
        let _ = di := pos in
        num
    with Stack.Empty ->
      0

  (** Return the top element in the stack. *)
  method topStack = Stack.top !stack

  (** Clear the stack. *)
  method clearStack = Stack.clear !stack

  (**
     * Push ([start, []], stmts, cts, brs) into the stack where start is the
     * start marker of a loop in the output, stmts is the IDs of the loop body
     * in the input, cts is the IDs of the continue statements (starting from
     * ctopt), and brs is the IDs of the break statements (starting from bropt).
     * Note that the statement start must be unique.
     *
     * Note: After processing (making one return, making CFG, etc.) a file
     * twice, the continue statement and the break statement stored in a Loop
     * stmtkind will not match the target statement in a Goto stmtkind for
     * continuation and breaking.
  *)
  method pushStack start block ctopt bropt =
    (* Collect statements until the first non-empty statement (included) after stmt is reached. *)
    let calc stmt =
      let rec helper cts stmt =
        match stmt.skind, stmt.succs with
          Instr [], next::[] -> helper (stmt.sid::cts) next
        | _ -> stmt.sid::cts in
      helper [] stmt in
    let apply_opt f def opt =
      match opt with
        None -> def
      | Some x -> f x in
    Stack.push ([start, []], List.map (fun stmt -> stmt.sid) (all_statements_block block), apply_opt calc [] ctopt, apply_opt calc [] bropt) !stack

  (** Push a dummy layer to collect decision vectors. *)
  method pushDummy =
    let start = mkEmptyStmt () in
    let _ = start.sid <- -1 in
    Stack.push ([start, []], [], [], []) !stack

  (**
     * Pop the top element from the stack. If the popped element corresponds to
     * an inner loop, add the decisions for the inner loop to the outer loop.
  *)
  method popStack =
    let ((pairs, _, _, _) as res) = Stack.pop !stack in
    let (longest, _) = List.fold_left (
      fun (longest, max) (start, decs_rev) ->
        let len = List.length decs_rev in
        if len > max then
          (decs_rev, len)
        else
          (longest, max)
    ) ([], 0) pairs in
    let _ =
      try
        let (pairs', stmts', cts', brs') = Stack.pop !stack in
        Stack.push (List.map (fun (start', decs_rev') -> (start', longest @ decs_rev')) pairs', stmts', cts', brs') !stack
      with Stack.Empty ->
        () in
    res

  (** Push a new start marker of a loop to the top element of the stack. *)
  method pushMarker start =
    try
      let (pairs, stmts, cts, brs) = Stack.pop !stack in
      Stack.push ((start, [])::pairs, stmts, cts, brs) !stack
    with Stack.Empty ->
      ()

  (**
     * Pop all the markers in the top element of the stack. If the popped
     * element corresponds to an inner loop, add the decisions for the inner
     * loop to the outer loop.
  *)
  method popMarkers =
    let ((_, stmts, cts, brs) as res) = self#popStack in
    let _ = Stack.push ([], stmts, cts, brs) !stack in
    res

  (** Push a decision consumed in a loop corresponding to the top element of the stack. *)
  method pushLoopDecision b =
    try
      let (pairs, stmts, cts, brs) = Stack.pop !stack in
      Stack.push (List.map (fun (start, decs_rev) -> (start, b::decs_rev)) pairs, stmts, cts, brs) !stack
    with Stack.Empty ->
      ()

  (** Push decisions consumed in a loop corresponding to the top element of the stack. *)
  method pushLoopDecisions bs =
    try
      let (pairs, stmts, cts, brs) = Stack.pop !stack in
      Stack.push (List.map (fun (start, decs_rev) -> (start, List.rev_append bs decs_rev)) pairs, stmts, cts, brs) !stack
    with Stack.Empty ->
      ()

  (** Return the decisions in the top element of the stack. *)
  method hasLoopDecision =
    let (pairs, _, _, _) = Stack.top !stack in
    List.exists (fun (_, decs_rev) -> decs_rev <> []) pairs

  (** Check if a statement is the start of the loop body corresponding to the top element of the stack. *)
  method isLoopStart stmt =
    try
      let (_, _, cts, _) = Stack.top !stack in
      List.mem stmt.sid cts
    with Stack.Empty ->
      false

  (** Check if a statement is the exit of the loop corresponding to the top element of the stack. *)
  method isLoopExit stmt =
    try
      let (_, _, _, brs) = Stack.top !stack in
      List.mem stmt.sid brs
    with Stack.Empty ->
      false

  method isInLoop stmt =
    try
      let (_, stmts, _, _) = Stack.top !stack in
      List.mem stmt.sid stmts
    with Stack.Empty ->
      false

  (** Pops the stack until the specified statement is reached or a dummy layer is reached. *)
  method popStackTo stmt =
    let has_dummy pairs = List.exists (fun (start, _) -> start.sid == -1) pairs in
    try
      let (pairs, stmts, _, _) = Stack.top !stack in
      if not (List.mem stmt.sid stmts) && not (has_dummy pairs) then
        let _ = self#popStack in
        self#popStackTo stmt
    with Stack.Empty ->
      ()
end

let not_empty_stmt stmt =
  match stmt.labels with
    [] ->
      begin
        match stmt.skind with
          Instr [] -> false
        | Block block when block.bstmts = [] && block.battrs = [] -> false
        | _ -> true
      end
  | _ -> true

(** Repeat the last statements num times. The repeated statements start with the specified statement start. *)
let repeatLoop (fd : fundec) (start : stmt) (num : int) =
  let rec split stmt head tail =
    match tail with
      [] -> (head, tail)
    | hd::tl when hd.sid = stmt.sid -> (hd::head, tl)
    | hd::tl -> split stmt (hd::head) tl in
  let (lbody, fbody_rev) = split start [] fd.sbody.bstmts in
  let var = (Var (makeTempVar fd intType), NoOffset) in
  let repeated =
    let start : stmt list = [mkStmt (Instr [Set (var, zero, locUnknown)])] in
    let guard = BinOp (Le, Lval var, integer num, intType) in
    let next = [mkStmt (Instr [Set (var, BinOp (PlusA, Lval var, one, intType), locUnknown)])] in
    mkFor start guard next (List.filter not_empty_stmt lbody) in
  let _ = fd.sbody.bstmts <- (List.rev_append repeated fbody_rev) in
  ()

let rec build_trace_instr helper instr =
  match instr with
    Call (res, Lval (Var fv, NoOffset), args, loc) when fv.vname = verifier_error_name ->
      let _ = helper#setErrorEncountered in
      dummyInstr
  | Call (res, Lval (Var fv, NoOffset), args, loc) ->
    begin
      try
        let ifundec = helper#getInputFundec fv.vname in
        let ofundec = helper#getOutputFundecRenamed fv.vname in
        (* Push a dummy layer to the stack because statement IDs are only unique
           in a function. If we don't push a dummy layer, some statement in the
           function body may be incorrectly identified as a loop start. *)
        let _ = if !compress > 0 then helper#pushDummy in
        let _ = build_trace_fundec helper ifundec ofundec in
        let _ = if !compress > 0 then ignore(helper#popStack) in
        Call (res, Lval (Var ofundec.svar, NoOffset), args, loc)
      with Not_found ->
        instr
    end
  | _ -> instr

and build_trace_stmt (helper : helperClass) ifun ofun istmt =
  let add_stmt stmt = ofun.sbody.bstmts <- stmt::ofun.sbody.bstmts in
  let hdopt xs = match xs with [] -> None | hd::_ -> Some hd in
  let newmarker () =
    let stmt = mkEmptyStmt () in
    let _ = stmt.sid <- new_sid () in
    let _ = add_stmt stmt in
    stmt in
  (* Detect loops and try to repeat the loop body with a for-loop. *)
  let _ =
    if !compress > 0 && helper#isLoopStart istmt then
      (* Try to advance if this is not the first time reaching the start of a loop. *)
      if helper#hasLoopDecision then
        let num = helper#advance in
        if num > 0 then
          match helper#popMarkers with
            ([(loop_start, _)], _, _, _) ->
              (* Repeat the statements. *)
              let _ = repeatLoop ofun loop_start num in
              (* Start the recording of new loops. *)
              let _ = helper#pushMarker (newmarker()) in
              ()
          | _ -> assert(false)
        else if !compress > 1 then
          helper#pushMarker (newmarker())
  in
  if helper#isContinueAfterLastDecision || helper#hasNextDecision then
    (* Add necessary statements and find the next statement to be processed. *)
    let next_stmt =
      match istmt.skind with
        If (cond, _, _, _) ->
          begin
            if not helper#hasNextDecision then
              let _ = helper#setFinished true in
              let _ =
                if helper#isInsertFinalError then
                  add_stmt (mkStmt (Instr [mkErrorInstr ()])) in
              None
            else
              let dec = helper#popNextDecision in
              (* Remember this decision if it is in a loop. *)
              let _ = if !compress > 0 then helper#pushLoopDecision dec in
              (* Add an assume command. *)
              let _ =
                let exp = if dec then cond else UnOp (LNot, cond, typeOf cond) in
                add_stmt (mkStmt (Instr [mkAssume exp])) in
              (match dec, istmt.succs with
                true, _::next::_ -> Some next
              | false, next::_ -> Some next
              | _, _ -> None)
          end
      | Instr instrs ->
        let instrs = List.map (build_trace_instr helper) instrs in
        let _ = add_stmt (mkStmt (Instr instrs)) in
        hdopt istmt.succs
      | Block block -> hdopt block.bstmts
      | Loop (block, _, ctopt, bropt) ->
        (* Start to record the decisions consumed in the new loop. *)
        let _ =
          if !compress > 0 then
            helper#pushStack (newmarker()) block ctopt bropt in
        hdopt istmt.succs
      | Goto (target, _) ->
        (* Pop the stack if goto the break statement of the loop on the top of the stack.
           Otherwise, always clear the stack if go to a statement outside the loop. *)
        let _ =
          if !compress > 0 then
            if helper#isLoopExit !target then
              ignore(helper#popStack)
            else if not (helper#isInLoop !target) then
              helper#popStackTo !target in
        hdopt istmt.succs
      | ComputedGoto _
      | Break _
      | Continue _ -> hdopt istmt.succs
      | Switch _ -> failwith("Switch is not supported.")
      | _ ->
        let _ = add_stmt istmt in
        hdopt istmt.succs in
    (* Make a recursive call at the end to avoid stack overflow. *)
    match next_stmt with
      None -> ()
    | Some next -> build_trace_stmt helper ifun ofun next
  else if not helper#isFinished then
    let _ = helper#setFinished true in
    begin
      if helper#isInsertFinalError then
        add_stmt (mkStmt (Instr [mkErrorInstr ()]))
    end
  else
    ()

and build_trace_block helper ifun ofun block =
  match block.bstmts with
    [] -> ()
  | next::_ -> build_trace_stmt helper ifun ofun next

and build_trace_fundec helper ifun ofun =
  (* Create the trace in the output function. After this, the statements in
     the output function are stored in a reversed order. *)
  let _ = build_trace_block helper ifun ofun ifun.sbody in
  (* Reverse statements in the function body and remove empty statements. *)
  let _ = ofun.sbody.bstmts <- List.rev (List.filter not_empty_stmt ofun.sbody.bstmts) in
  ()

let get_fundecls globals =
  List.flatten (List.map (fun global ->
    match global with
      GFun (fd, _) -> [GVarDecl (fd.svar, locUnknown)]
    | _ -> []
  ) globals)

let insert_fundecls globals =
  let fdecls = get_fundecls globals in
  let rec helper visited globals =
    match globals with
      [] -> visited@fdecls
    | (GFun _)::_ -> visited@fdecls@globals
    | hd::tl -> helper (visited@[hd]) tl in
  helper [] globals

let reorder_trace helper =
  let input = helper#getInput in
  let output = helper#getOutput in
  (* Sort globals. *)
  let _ =
    (* A comparsion function for globals in the input file. *)
    let icompare =
      (* Returns the index of a global in the global list of the input file. *)
      let get_index =
        let gmap =
          let m = Hashtbl.create (List.length input.globals) in
          let _ = List.iteri (fun i g -> Hashtbl.add m g i) input.globals in
          m in
        let fmap =
          let m = Hashtbl.create (List.length input.globals) in
          let _ = List.iteri (fun i g -> match g with GFun (fd, _) -> Hashtbl.add m fd.svar.vname i | _ -> ()) input.globals in
          m in
        fun g ->
          match g with
            GFun (fd, _) ->
              begin
                try
                  Hashtbl.find fmap fd.svar.vname
                with Not_found -> failwith("Failed to find the index of function: " ^ fd.svar.vname)
              end
          | _ -> Hashtbl.find gmap g in
      fun g1 g2 ->
        let i1 = get_index g1 in
        let i2 = get_index g2 in
        if i1 > i2 then
          1
        else if i1 < i2 then
          -1
        else
          0 in
    let origin g =
      match g with
        GFun (fd, loc) when has_suffix fd.svar.vname -> GFun (copyFunction fd (remove_suffix fd.svar.vname), loc)
      | _ -> g in
    let ocompare g1 g2 =
      let c = icompare (origin g1) (origin g2) in
      match c, g1, g2 with
        0, GFun (fd1, _), GFun (fd2, _) -> - (compare fd1.svar.vname fd2.svar.vname)
      | 0, _, _ when g1 <> g2 -> failwith("Two globals have the same index but they are not the same.")
      | _ -> c in
    output.globals <- List.sort ocompare output.globals
  in
  let _ = output.globals <- insert_fundecls output.globals in
  ()

let add_verifier_error_decl file =
  let vi = makeVarinfo true verifier_error_name (TFun (TVoid [], None, false, [])) in
  let _ = vi.vattr <- [Attr ("noreturn", [])] in
  let _ = vi.vstorage <- Extern in
  let decl = GVarDecl (vi, locUnknown) in
  let _ = file.globals <- decl::file.globals in
  ()

let add_verifier_assume_decl file =
  let return_type = TVoid [] in
  let arg_types = [("expr", TInt (IInt, []), [])] in
  let vi = makeVarinfo true verifier_assume_name (TFun (return_type, Some arg_types, false, [])) in
  let decl = GVarDecl (vi, locUnknown) in
  let _ = file.globals <- decl::file.globals in
  ()

let add_except_func helper fname =
  let input = helper#getInput in
  let output = helper#getOutput in
  let has_func fname =
    List.exists (
      fun global ->
        match global with
          GFun (fd, _) -> fd.svar.vname = fname
        | _ -> false
    ) output.globals in
  let add_func global = output.globals <- global::output.globals in
  List.iter (
    fun global ->
      match global with
        GFun (fd, _) when has_func fd.svar.vname || fd.svar.vname = fname -> ()
      | GFun _ -> add_func global
      | _ -> ()
  ) input.globals

module Vars =
struct
  type t = varinfo
  let compare = Pervasives.compare
end
  
module VS = Set.Make(Vars)

let insert_missing_fundecs helper =
  let input = helper#getInput in
  let output = helper#getOutput in
  let is_missing_in v file =
    List.for_all (
      fun global ->
        match global with
          GFun (fd, _) when fd.svar.vname = v.vname -> false
        | _ -> true
    ) file.globals in
  let get_dereferences file =
    let set = ref VS.empty in
    let visitor = object(self)
      inherit nopCilVisitor
        
      method vexpr e =
        match e with
          AddrOf (Var v, _) ->
            let _ = set := VS.add v !set in
            DoChildren
        | _ -> DoChildren
    end in
    let _ = Cil.visitCilFileSameGlobals visitor file in
    VS.elements !set in
  let add_missing vs =
    let fmap = get_fundec_map input in
    let fds = List.map (
      fun v ->
        try
          Hashtbl.find fmap v.vname
        with Not_found ->
          failwith("Failed to find in the output the function " ^ v.vname ^ " that appears in the input.")
    ) vs in
    List.iter (fun fd ->
      if is_missing_in fd.svar output then
        output.globals <- (GFun (fd, locUnknown))::output.globals
    ) fds in
  let filter_in_input vs = List.filter (fun v -> not (is_missing_in v input)) vs in
  add_missing (filter_in_input (get_dereferences output))

let error_reachable file ?fname:(fname=main_name) decisions =
  let helper = new helperClass file decisions in
  let _ = helper#setInsertFinalError false in
  let _ = helper#setContinueAfterLastDecision true in
  let imain =
    try
      helper#getInputFundec fname
    with Not_found ->
      failwith("Failed to find the function " ^ fname ^ ".") in
  let omain = helper#getOutputFundec fname in
  let _ = build_trace_fundec helper imain omain in
  (*let _ = add_except_func helper fname in*)
  let _ = insert_missing_fundecs helper in
  let _ = reorder_trace helper in
  helper#isErrorEncountered && not helper#hasNextDecision
    
let trace file ?fname:(fname=main_name) decisions =
  let helper = new helperClass file decisions in
  let imain =
    try
      helper#getInputFundec fname
    with Not_found ->
      failwith("Failed to find the function " ^ fname ^ ".") in
  let omain = helper#getOutputFundec fname in
  let _ = build_trace_fundec helper imain omain in
  (*let _ = add_except_func helper fname in*)
  let _ = insert_missing_fundecs helper in
  let _ = reorder_trace helper in
  if helper#isErrorEncountered || helper#hasNextDecision then
    None
  else
    Some (helper#getOutput)
