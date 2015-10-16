
let keys m = Hashtbl.fold (fun k v res -> k::res) m []

let values m = Hashtbl.fold (fun k v res -> v::res) m []

(** Adds y to h(x) if there is some z in h(x) with y in h(z) where h is a hash table. *)
let saturate h =
  let changed = ref true in
  let union l1 l2 =
    List.fold_left (
      fun l x -> 
        if List.mem x l then 
          l 
        else 
          let _ = changed := true in
          x::l
    ) l1 l2 in
  let helper h x =
    Hashtbl.replace h x (
      List.fold_left (
        fun res y -> 
          try 
            union res (Hashtbl.find h y) 
          with Not_found -> 
            res
      ) (Hashtbl.find h x) (Hashtbl.find h x)
    ) in
  let _ = 
    while !changed do
      let _ = changed := false in
      List.iter (helper h) (keys h)
    done in
  h
