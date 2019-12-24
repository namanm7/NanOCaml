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

(** [recognized_spl_chars] is the list of all recognzied special characters*)
let recognized_spl_chars = ["~"; "`"; "!"; "@"; "#"; "$"; "%"; "^"; "&"; "*"; 
                            "("; ")"; "-"; "_"; "="; "+"; "\\"; "|"; "}"; "]"; 
                            "{"; "["; "]"; ";"; ":"; "'"; "\""; ","; "<"; "."; 
                            ">"; "/"; "?"]

(** [recognized_chars] is the regular expression representing all recognized
    alphanumeric characters*)
let recognized_chars = Str.regexp "[A-Za-z0-9]"

(** [verify_input s] is Chracter [s] if [s] is a recognized alphanumeric 
    character or special character, and Ignore otherwise*)
let verify_input s = 
  let first_char = Str.first_chars s 1 in 
  if Str.string_match recognized_chars first_char 0 ||
     List.mem first_char recognized_spl_chars
  then Character (first_char) else Ignore

let translate s = 
  match s with 
  | "\002\000\000\000\000\000" -> Ignore
  | "\027[A\000\000\000" -> Up
  | "\027[B\000\000\000" -> Down
  | "\027[C\000\000\000" -> Right
  | "\027[D\000\000\000" -> Left
  | "\027a\000\000\000\000" -> BOL
  | "\027d\000\000\000\000" -> EOL
  | "\027w\000\000\000\000" -> TOP  
  | "\027s\000\000\000\000" -> BTM
  | "\027j\000\000\000\000" -> Jump 
  | "\027t\000\000\000\000" -> Spellcheck
  | "\027p\000\000\000\000" -> Portal
  | "\027<\000\000\000\000" -> PortalJumpUp
  | "\027>\000\000\000\000" -> PortalJumpDown
  | "\203\152\000\000\000\000" -> PortalJumpDown
  | "\024\000\000\000\000\000" -> Quit
  | "\127\000\000\000\000\000" -> Backspace
  | "\027x\000\000\000\000" -> Cut 
  | "\027c\000\000\000\000" -> Copy
  | "\027v\000\000\000\000" -> Paste
  | " \000\000\000\000\000" -> Space
  | "\n\000\000\000\000\000" -> Nextline (**CHANGE NAME OF LETTER TO MATCH SEMANTIC MEANING *)
  | "\t\000\000\000\000\000" -> Tab
  | "\006\000\000\000\000\000" -> FindAndReplace
  | "\014\000\000\000\000\000" -> NewDoc
  | "\021\000\000\000\000\000" -> Undo
  | "\018\000\000\000\000\000" -> Redo
  | "\008\000\000\000\000\000" -> Help
  | "\016\000\000\000\000\000" -> Save
  | s -> verify_input s