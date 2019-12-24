(** 
   Translation of user keypresses to specific commands
*)

(** [key_event] is the abstract type representing all recognized key events *)
type keyevent = 
  | Character of string
  | Nextline
  | Tab
  | Backspace
  | Space
  | Quit 
  | Up
  | Down
  | Right
  | Left
  | Ignore
  | FindAndReplace
  | NewDoc
  | Undo
  | Redo
  | Help
  | BOL
  | EOL
  | TOP 
  | BTM
  | Jump
  | Save
  | Spellcheck 
  | Portal
  | PortalJumpUp
  | PortalJumpDown
  | Cut
  | Copy 
  | Paste

(** [translate s] is the keyevent represented by [s]*)
val translate : string -> keyevent