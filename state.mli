(** 
   Representation of dynamic textfile state.

   This module represents the state of a textfile as it is being updated, 
   including a Buffer that contains all the text that will eventually be 
   written to the file.
*)

(** [t] is the abstract type of state with all the record fields defined below*)
type t 

(** [undoStack] is a Stack of all possible states that can be undone*)
val undoStack: t Stack.t

(** [redoStack] is a Stack of all states that have been undone and can be 
    redone *)
val redoStack: t Stack.t

(** [copy st] is a copy of [st] with the array copied to prevent issues with 
    mutability*)
val copy: t -> t

(** [init_state txtfile] is the intial state with contents from [txtfile]*)
val init_state: TextFile.t -> t                     

(** [add_string st s row col] is a state with [s] added in row [row] and column
    [col]*)
val add_string: t -> string -> int -> int -> t 

(** [remove_char st row col] is a state with the character at [row] [col] 
    removed*)
val remove_char: t -> int -> int -> t 

(** [current_text st] is the contents of [st] concatenated together line by 
    line with '\n' as the seperator UNUSED*)
val current_text: t -> string

(** [current_text_lines st] is the array of lines that is currently printed on
    screen from [st]*)
val current_text_lines: t -> string array

(** [write st] writes the contents of [st] to the textfile in [st]*)
val write: t -> unit 

(** [find_and_replace st find replace] is a new [st] with [find] replaced by 
    [replace] in the contents of the buffer of st*)
val find_and_replace: t -> string -> string -> t (** *)

(** [get_voff st] is the vertical offset of [st]*)
val get_voff: t -> int 

(** [get_textfile st] is the textfile of st*)
val get_textfile: t -> TextFile.t

(** [get_hoff st] is the horizontal offset of [st]*)
val get_hoff: t -> int 

(** [get_cursor_pos st] is the tuple of the cursor position of [st]*)
val get_cursor_pos: t -> int * int 

(** [get_portals st] is the int list representing the lines for portals of 
    [st]*)
val get_portals: t -> int list

(** [move_cursor_up st] is the state after the cursor has attempted to move up 
    one unit in [st]*)
val move_cursor_up: t -> t (** *)

(** [move_cursor_down st] is the state after the cursor has attempted to move  
    down one unit in [st]*)
val move_cursor_down: t -> t (** *)

(** [move_cursor_left st] is the state once the user has pressed the left arrow 
    key in [st]*)
val move_cursor_left: t -> t (** *)

(** [move_cursor_right st] is the state once the user has pressed the right 
    arrow key in [st]*)
val move_cursor_right: t -> t (** *)

(** [add_nextline st] is the new state once the user has pressed the return/
    enter key in [st]*)
val add_nextline: t -> t (** *)

(** [add_tab st] is the new state once the user has pressed the tab key in 
    [st]*)
val add_tab: t -> t (** *)

(** [add_char st s] is the new state once the user has pressed the [s] key in 
    [st]
    Requires: [s] is a single character string*)
val add_char: t -> string -> t (** *)

(** [handle_backspace st] is the state once the backspace/delete key has been 
    pressed in [st]*)
val handle_backspace: t -> t (** *)

(** [handle_undo st] is the previous state before the last change was made. Adds
    the current state to the redo stack and returns [st] if nothing
    can be undone*)
val handle_undo: t -> t (** *)

(** [handle_redo st] is the last state that was undone, if and only if no new 
    changes have since been made to [st]. If changes have been made, this is 
    [st]*)
val handle_redo: t -> t (** *)

(** [clear_redo ()] clears the redo stack*)
val clear_redo: unit -> unit (** *)

(** [move_bol st] is the state with the cursor position moved all the way left 
    to the beginning of the current line*)
val move_bol: t -> t 

(** [move_eol st] is the state with the current position moved all the way right
    to the end of the current line*)
val move_eol: t -> t

(** [move_top st] is the state with the current position moved all the to the 
    top left*)
val move_top: t -> t

(** [move_btm st] is the state with the current positon moved all the way to the 
    bottom of the file*)
val move_btm: t -> t 

(** [jump st target_line] is [st] with the cursor at [target_line] *)
val jump: t -> int -> t 

(** [toggle_portal st] is [st] with the current line on as a portal if it were 
    not already a portal or with the current line off as a portal if it were 
    already a portal *)
val toggle_portal: t -> t

(** [portal_jump_up st] is [st] with the cursor jumped up to the next portal, if
    it exists*)
val portal_jump_up: t -> t

(** [portal_jump_down st] is [st] with the cursor jumped down to the last
    portal, if it exists*)
val portal_jump_down: t -> t

(** [cut st (start,stop)] is [st] with the lines in [st] from start to stop
    removed and added to the clipboard*)
val cut: t -> int * int -> t

(** [copy st (start,stop)] is [st] with the lines in [st] from start to stop
    added to the clipboard*)
val handle_copy: t -> int * int -> t

(** [paste st target] is the state with the contents of the clipboard pasted at
    line number [target] in [st], if target is a valid line number. If not,
    no change is made to [st]*)
val paste: t -> int -> t

(** [read_from_file ic bufferarry line] is a tuple of a string array containing
    all the lines from the file from [ic] and the number of lines in the file*)
val read_from_file: in_channel -> string array -> int -> string array * int

(** [get_length st] is the length of [st]*)
val get_length: t -> int

(** [help_state txtfile] is the initialization of the help text, given by 
    "help.txt"*)
val help_state: TextFile.t -> t