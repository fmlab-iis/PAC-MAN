
type trace

val compress : int ref
  
val typesig_from_actuals : bool ref

(** Parses a decision vector from a 0-1 string. *)
val decisions_from_string : string -> bool array

(** Returns true if the given trace is empty, i.e., the trace corresponding to a decision vector is invalid. *)
val is_trace_empty : trace -> bool

(** Prints a file on the screen. *)
val pFile : Cil.file -> unit

(** Prints a trace on the screen. *)
val pTrace : trace -> unit

(** Saves a trace to a file. *)
val save_trace : trace -> string -> unit

(** Remove function pointers. *)
val remove_function_pointers : Cil.file -> unit

(** Replaces assert by __VERIFIER_error. *)                          
val replace_assert : Cil.file -> unit

(** Inserts a declaration of __VERIFIER_error() at the beginning of a file. *)
val add_verifier_error_decl : Cil.file -> unit

(** Inserts a declaration of __VERIFIER_assume() at the beginning of a file. *)
val add_verifier_assume_decl : Cil.file -> unit

(** Returns the trace decided by a sequence of branch decisions. *)
val trace : Cil.file -> ?fname:string -> bool array -> trace

(** Inlines nonrecursive functions. *)
val inline : Cil.file -> unit

val error_reachable : Cil.file -> ?fname:string -> bool array -> bool
