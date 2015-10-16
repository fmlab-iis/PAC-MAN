
(** Adds y to h(x) if there is some z in h(x) with y in h(z) where h is a hash table. *)
val saturate : ('a, 'a list) Hashtbl.t -> ('a, 'a list) Hashtbl.t
