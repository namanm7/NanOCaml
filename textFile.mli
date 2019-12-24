(** 
   Representation of a text file 

   This module represents a text file that has been opened/created for editing.
*)

(** [t] is the abstract type representing a textfile*)
type t 

(** [get_name file] is the name of [file]*)
val get_name: t -> string

(** [init_file name] is the TextFile of [name]*)
val init_file: string -> t 

(** [get_input t] is the in_channel of [t]*)
val get_input: t -> in_channel

(** [write t s] writes [s] to the file of [t]*)
val write: t -> string -> unit