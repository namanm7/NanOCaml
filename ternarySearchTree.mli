(** 
   A ternary search tree to be used for spell-checking
*)

(** [t] is the type of a ternary search tree*)
type t

(** [empty] is an empty Ternary Search Tree*)
val empty: t 

(** [insert s tst] inserts [s] into [tst] iff s is not the empty string*)
val insert: string -> t -> t 

(** [search s tst] is true if [s] belongs in [tst], or if [s] removed of "-" and
    "\\" and both portions are in [tst], false otherwise*)
val search: string -> t -> bool 