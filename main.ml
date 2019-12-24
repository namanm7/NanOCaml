open ANSITerminal
open TextFile
open State
open KeyPress                                                                                       
open TernarySearchTree                                                         

(** THIS WAS TAKEN FROM performanceTest.ml FROM A5! 
    [shuffle lst] is a random permutation of [lst]. *)
let rec shuffle (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [extract_keys_from_assoc lst alst] creates a list of keys from [alst] and 
    then shuffles them, with [lst] as the accumulator. *)
let rec extract_keys_from_assoc lst alst = 
  match alst with
  | [] ->  shuffle lst 
  | (x,y)::t -> extract_keys_from_assoc (x::lst) t

(** [build_dictionary dict lst] populates [dict] with each element of [lst]*)
let rec build_dictionary dict lst =
  match lst with
  | [] -> dict
  | h::t -> build_dictionary (TernarySearchTree.insert h dict) t 

(** [create_dictionary] is a full dictionary with words from 
    words_dictionary.json, shuffled *)
let create_dictionary =
  let dictionary = TernarySearchTree.empty in
  "words_dictionary.json" |> Yojson.Basic.from_file 
  |>  Yojson.Basic.Util.to_assoc 
  |> extract_keys_from_assoc [] |> build_dictionary dictionary


(** [help] is the state representing the contents of "help.txt"*)
let help = "help.txt" |> TextFile.init_file |> State.help_state

(** [dictionary] is the literal dictionary used for spellcheck*)
let dictionary = create_dictionary 

(** [spellcheck] keeps track of whether the spellcheck feature is on or off*)
let spellcheck = ref false

(** [create_space x acc] is (str,str) where str is [acc] with [x] number of " " 
    (space strings) concatenated to the end of it*)
let rec create_space x acc =
  match x with
  | 0 -> (acc,acc)
  | _ -> create_space (x-1) (acc^" ")

(** [draw_title width unedited] prints a title to the top of the terminal with
    width [width] and notification that it is edited if [unedited] is false*)
let draw_title width unedited=
  ANSITerminal.set_cursor 1 1;
  for i = 1 to width do
    ANSITerminal.print_string [on_red] " ";
  done; 
  ANSITerminal.move_bol(); ANSITerminal.move_cursor 0 0;
  ANSITerminal.print_string [white; on_red; Bold] "^H: Get Help"; 
  let padding = ((width - 14)/2) in 
  ANSITerminal.move_cursor (padding-13) 0; 
  ANSITerminal.print_string [white; on_red; Bold] 
    ("NanOCaml 1.0.3" ^ (if unedited then "" else " (edited) ")); 
  ANSITerminal.move_bol(); ANSITerminal.move_cursor 0 1; ()

(** [enter_raw_mode ()] sets up the terminal to not echo any text input and to
    read only one character at once*)
let enter_raw_mode () = 
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in 
  terminal.c_icanon <- false; 
  terminal.c_echo <- false; 
  terminal.c_verase <- '\002';
  terminal.c_vmin <- 1; 
  terminal.c_vtime <- 1;
  Unix.tcsetattr input Unix.TCSANOW terminal; ()

(** [reset_for_exit ()] resets the terminal to its state before NanOCaml ran*)
let reset_for_exit () = 
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in 
  terminal.c_icanon <- true; 
  terminal.c_echo <- true;
  terminal.c_verase <- '\127';
  Unix.tcsetattr input TCSANOW terminal; 
  ANSITerminal.erase Screen; ANSITerminal.set_cursor 1 1; ()

(** [word_by_word words_in_line_string acc width dict spellcheck] looks at each 
    of the elements in [words_in_line_string] and prints it red if it does not 
    belong in [dict], if and only if [spellcheck] is true*)
let rec word_by_word words_in_line_string acc width =
  match words_in_line_string with 
  | [] -> ()
  | h::t -> 
    let new_acc = acc + (1 + String.length h) in 
    let membership = if !spellcheck = false then true 
      else TernarySearchTree.search h dictionary in 
    if new_acc <= width then 
      (if membership = false then 
         (ANSITerminal.print_string [red] (h^" "); 
          word_by_word t new_acc width) 
       else (ANSITerminal.print_string [white] (h^" "); 
             word_by_word t new_acc width))
    else (if membership = false then 
            ANSITerminal.print_string [red] (Str.string_before h (width-acc)) 
          else 
            ANSITerminal.print_string [white] (Str.string_before h (width-acc)))



(**[to_list s index ans] takes [s] and returns [ans] containing all the 
   individual characters in [s]. 
   Requires:
   [s] is a string
   [index] is an int
   [ans] is a char list  *)
let rec to_list (index:int) (ans:char list) (s:string)  : char list=
  if index < String.length s then 
    to_list (index + 1) ((String.get s index)::ans) s
  else List.rev ans 

(** [print_line_number acc color] prints the line number specified by [acc]
    in [color]*)
let print_line_number acc color = 
  ANSITerminal.print_string [color]  
    (if acc<10 
     then ("    "^(Int.to_string acc)) 
     else if acc < 100 
     then ("   "^(Int.to_string acc)) 
     else if acc<1000 
     then ("  "^(Int.to_string acc)) 
     else if acc<10000 
     then (" "^(Int.to_string acc))
     else Int.to_string acc); 
  ANSITerminal.print_string [white] " "; ()

(** [create_numbered_lines_word_by_word lst dict acc size voff hoff] prints each
    line of text in [lst] with proper number lines*)
let create_numbered_lines_word_by_word lst acc size voff 
    hoff portals = 
  let acc = ref (voff+1) in
  let length = Array.length lst in 
  let from = voff in 
  let end_at = if((length-(voff + (snd size))) < 0 ) then length-2 else 
      voff + (snd size) - 2 in  
  for i = from to end_at do
    let number_color = if List.mem i portals then magenta else cyan in 
    print_line_number !acc number_color;
    word_by_word (if (Array.get lst i) = "" 
                  || hoff > String.length (Array.get lst i) 
                  then [""] 
                  else Str.string_after (Array.get lst i) hoff 
                       |> String.split_on_char ' ' ) 6 (fst size); 
    if !acc+1 < (snd size)+voff && !acc > voff 
    then ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
    acc := !acc+1
  done;
  let number_color = if List.mem (length-1) portals then magenta else cyan in 
  if !acc+1 <= (snd size)+voff && !acc > voff 
  then (print_line_number !acc number_color; word_by_word 
          (if (Array.get lst ((length) -1)) = "" 
           || hoff > String.length (Array.get lst (length -1)) 
           then [""]
           else Str.string_after (Array.get lst (length -1)) hoff 
                |> String.split_on_char ' ' ) 6 (fst size); ())

(** [prompt_find_and_replace ()] asks the user for two strings, what to find
    and what to replace, and is these two strings as a tuple*)
let prompt_find_and_replace () = 
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in
  terminal.c_icanon <- true; 
  terminal.c_echo <- true;
  terminal.c_verase <- '\127';
  Unix.tcsetattr input TCSANOW terminal; 
  let width, height = ANSITerminal.size() in 
  ANSITerminal.set_cursor 1 (height - 3); ANSITerminal.erase Below; 
  for i = 1 to width do 
    ANSITerminal.print_string [on_red] " ";
  done;
  ANSITerminal.move_bol(); 
  ANSITerminal.print_string [on_red; white] "Find: ";
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  let find = read_line () in 
  ANSITerminal.set_cursor 1 (height - 1);
  for i = 1 to width do 
    ANSITerminal.print_string [on_red] " ";
  done;
  ANSITerminal.move_bol();
  ANSITerminal.print_string [on_red; white] "Replace: ";
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  let replace = read_line () in
  ANSITerminal.set_cursor 7 2; ANSITerminal.erase Below; 
  enter_raw_mode(); (find, replace)

(** [verify_string_to_int s] is the int representation of [s], if possible. If 
    not possible, it is 1*)
let verify_string_to_int s ver = 
  if String.length s >= 18 then (if ver = "Jump" then 1 else 0)
  else 
    try 
      (let convert = int_of_string(s) in 
       if convert <= 0 then (if ver = "Jump" then 1 else 0) else convert)
    with _ -> if ver = "Jump" then 1 else 0

(** [promt_save st] promts the user if they would like to save [st] and executes
    the command*)
let rec prompt_save st =
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in 
  terminal.c_icanon <- true; 
  terminal.c_echo <- true;
  terminal.c_verase <- '\127';
  Unix.tcsetattr input TCSANOW terminal; 
  let width, height = ANSITerminal.size() in 
  ANSITerminal.set_cursor 1 (height - 2); 
  ANSITerminal.erase Below; 
  for i = 1 to width do 
    ANSITerminal.print_string [on_red] " ";
  done;
  ANSITerminal.move_bol();
  ANSITerminal.print_string [on_red; white] "Would You Like to Save? (Y/N): ";
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  match Stdlib.read_line () with 
  |s -> if String.lowercase_ascii s = "y" 
    then (State.write st;)
    else if String.lowercase_ascii s = "n"
    then ()
    else (prompt_save st)

(** [prompt_cut_copy()] is the couple representing the range of lines the user 
    has chosen to cut or copy*)
let prompt_cut_copy () = 
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in 
  terminal.c_icanon <- true; 
  terminal.c_echo <- true;
  terminal.c_verase <- '\127';
  Unix.tcsetattr input TCSANOW terminal; 
  let width, height = ANSITerminal.size() in 
  ANSITerminal.set_cursor 1 (height - 4);
  ANSITerminal.erase Below;
  for i = 1 to width do 
    ANSITerminal.print_string [on_red] " ";
  done;
  ANSITerminal.move_bol(); 
  ANSITerminal.print_string [on_red; white] "Start: ";
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  ANSITerminal.print_string [white] 
    "Ex: Start: 1 Stop: 1 targets only line 1. \
     Start: 19 Stop 78 targets lines 19 through 78";
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  let start = read_line () in 
  ANSITerminal.set_cursor 1 (height - 1);
  for i = 1 to width do 
    ANSITerminal.print_string [on_red] " ";
  done;
  ANSITerminal.move_bol(); 
  ANSITerminal.print_string [on_red; white] "Stop: ";
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  let stop = read_line () in
  ANSITerminal.set_cursor 7 2; ANSITerminal.erase Below; 
  enter_raw_mode();  
  let num_one = verify_string_to_int start "cut" in 
  let num_two = verify_string_to_int stop "cut" in 
  (num_one, num_two)

(** [prompt_paste ()] is the line number the user has requested to paste to or 
    move to once prompted*)
let prompt_paste_jump ver = 
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in 
  terminal.c_icanon <- true; 
  terminal.c_echo <- true;
  terminal.c_verase <- '\127';
  Unix.tcsetattr input TCSANOW terminal; 
  let width, height = ANSITerminal.size() in 
  ANSITerminal.set_cursor 1 (height - 1);
  ANSITerminal.erase Below;
  for i = 1 to width do 
    ANSITerminal.print_string [on_red] " ";
  done;
  ANSITerminal.move_bol();
  ANSITerminal.print_string [on_red; white] (ver^" to: ");
  ANSITerminal.move_cursor 0 1; ANSITerminal.move_bol();
  let target = read_line () in 
  enter_raw_mode(); 
  verify_string_to_int target ver

(** [run_help dict spellcheck st voff hoff] prints the help page to the screen*)
let rec run_help st voff hoff=
  let text = State.current_text_lines help in
  create_numbered_lines_word_by_word text 1 
    (ANSITerminal.size()) voff hoff [];
  enter_raw_mode();
  let width,height = ANSITerminal.size() in
  let bytes = Bytes.create 6 in 
  ignore(Unix.read (Unix.stdin) bytes 0 6);
  match KeyPress.translate (Bytes.to_string bytes) with
  |Up -> ANSITerminal.set_cursor 1 1;
    ANSITerminal.erase Below;
    if voff = 0 
    then run_help st voff hoff 
    else run_help st (voff-1) hoff
  |Down -> ANSITerminal.set_cursor 1 1;
    ANSITerminal.erase Below;
    if voff+snd (ANSITerminal.size())-1 = State.get_length help 
    then run_help st voff hoff 
    else if (height>State.get_length help)
    then run_help st (voff) hoff 
    else run_help  st (voff+1) hoff 
  |Left -> ANSITerminal.set_cursor 1 1;
    ANSITerminal.erase Below;if hoff = 0 
    then run_help st voff hoff 
    else run_help st (voff) (hoff-1)
  |Right -> ANSITerminal.set_cursor 1 1;
    ANSITerminal.erase Below;
    if hoff = 86- fst (ANSITerminal.size()) 
    then run_help st voff hoff 
    else if (width > 86) 
    then run_help st (voff) (hoff) 
    else run_help st (voff) (hoff+1)
  |Nextline -> ANSITerminal.set_cursor 1 2;
    ANSITerminal.erase Below; run_NanOCaml st
  |_-> ANSITerminal.set_cursor 1 1;
    ANSITerminal.erase Below; run_help st voff hoff


(** [run_NanOCaml dict st] prints the entire interface and handles a single 
    keystroke given by the user, with [dict] as the spellcheck dictionary and
    [st] as the current state*)
and run_NanOCaml st = 
  Printf.printf "\027[?25l%!";
  ANSITerminal.set_cursor 1 1;
  draw_title (fst (ANSITerminal.size())) (Stack.length (State.undoStack)= 1);
  ANSITerminal.set_cursor 1 2;
  let size = ANSITerminal.size() in 
  let text = State.current_text_lines st in 
  let voff = State.get_voff st in 
  let hoff = State.get_hoff st in 
  let portals = State.get_portals st in 
  create_numbered_lines_word_by_word text 1 size voff 
    hoff portals; 
  let x,y = State.get_cursor_pos st in 
  ANSITerminal.set_cursor x y;
  Printf.printf "\027[?25h%!";
  let bytes = Bytes.create 6 in 
  ignore(Unix.read (Unix.stdin) bytes 0 6);
  match KeyPress.translate (Bytes.to_string bytes) with
  | Ignore -> State.clear_redo(); run_NanOCaml st
  | Up -> run_NanOCaml (State.move_cursor_up st)
  | Down -> run_NanOCaml (State.move_cursor_down st)
  | Right -> run_NanOCaml (State.move_cursor_right st)
  | Left -> run_NanOCaml (State.move_cursor_left st)
  | Nextline -> State.clear_redo(); 
    run_NanOCaml (State.add_nextline st)
  | Tab -> State.clear_redo(); run_NanOCaml (State.add_tab st)
  | Character s -> State.clear_redo(); 
    run_NanOCaml (State.add_char st s)
  | Space -> State.clear_redo(); 
    run_NanOCaml (State.add_char st " ")
  | Backspace -> State.clear_redo(); 
    run_NanOCaml (State.handle_backspace st)
  | Cut -> State.clear_redo(); 
    run_NanOCaml (State.cut st (prompt_cut_copy ())) 
  | Copy -> run_NanOCaml (State.handle_copy st (prompt_cut_copy ())) 
  | Paste -> State.clear_redo(); 
    run_NanOCaml (State.paste st (prompt_paste_jump "Paste"))
  | Quit -> prompt_save st; reset_for_exit();
  | FindAndReplace -> State.clear_redo(); 
    begin match prompt_find_and_replace () with 
      | (find,replace) -> run_NanOCaml 
                            (State.find_and_replace st find replace) end
  | Jump -> run_NanOCaml (State.jump st (prompt_paste_jump "Jump"))
  | Undo -> run_NanOCaml (State.handle_undo st)
  | Redo -> run_NanOCaml (State.handle_redo st)
  | BOL -> run_NanOCaml (State.move_bol st)
  | EOL -> run_NanOCaml (State.move_eol st)
  | TOP -> run_NanOCaml (State.move_top st)
  | BTM -> run_NanOCaml (State.move_btm st)
  | Spellcheck -> spellcheck := not !spellcheck; run_NanOCaml st
  | Portal -> State.clear_redo(); 
    run_NanOCaml (State.toggle_portal st)
  | PortalJumpUp -> run_NanOCaml  (State.portal_jump_up st)
  | PortalJumpDown -> run_NanOCaml (State.portal_jump_down st)
  | NewDoc -> prompt_save st; reset_for_exit(); main () 
  | Help -> reset_for_exit (); run_help st 0 0
  | Save -> 
    prompt_save st; 
    enter_raw_mode();
    create_state (State.get_textfile st |>TextFile.get_name)

(** [create_state s] creates the initial state given by the file name [s] and 
    starts up the interface*)
and create_state s = 
  (* try  *)
  ANSITerminal.erase Above;
  (*draw_title (fst (ANSITerminal.size())) false;*)
  s |> TextFile.init_file |> State.init_state 
  |> run_NanOCaml

(** [create_new_file d_name] prompts the user for a file name and creates it in
    the directory [d_name]*)
and create_new_file d_name  =
  ANSITerminal.set_cursor 1 1;
  ANSITerminal.erase Below;
  Printf.printf "\027[?25h%!";
  ANSITerminal.print_string [white] 
    "Name of folder/document (only accepts .mli, 
     .ml, .txt. Be sure to add file type to name.): \n";
  ANSITerminal.print_string [white] "> ";
  let input = Unix.stdin in 
  let terminal = Unix.tcgetattr input in 
  terminal.c_icanon <- true; 
  terminal.c_echo <- true;
  terminal.c_verase <- '\127';
  Unix.tcsetattr input TCSANOW terminal; 
  match Stdlib.read_line () with 
  | s -> (try (if(((String.sub s (String.length s - 4) 4 = ".mli" || 
                    (String.sub s (String.length s - 4) 4 ) = ".txt") 
                   && String.length s >= 5) || 
                  ((String.sub s (String.length s - 3) 3) = ".ml" && 
                   String.length s >= 4))
               then  (enter_raw_mode(); loop_file d_name s)
               else create_new_file d_name)
          with exn -> create_new_file d_name)

(** [cut_list lst counter] is [lst] without the first [counter] elements *)
and cut_list lst counter =
  match lst with
  |[] -> []
  |h::t -> if (counter = 1)then lst else cut_list t (counter-1)

(** [page_list page lst] is the elements of [lst] that belong on the current 
    page*)
and page_list page lst =
  if page = 1 then lst else page_list (page-1) 
      (cut_list lst (snd (ANSITerminal.size()))) 

(** [format_directory page selected lst counter] prints the elements in the 
    current directory from [lst] on [page] with [selected] highlighted*)
and format_directory page selected lst counter =
  match lst with
  |[]-> ANSITerminal.print_string [white] "";
  |a::t -> if (counter = (snd (ANSITerminal.size ())-1)) then 
      (if (selected = counter) then 
         (ANSITerminal.print_string [black; on_white] a;) 
       else (ANSITerminal.print_string [white] a;))
    else (if(selected = counter) then 
            (ANSITerminal.print_string [black; on_white] (a^"\n"))
          else (ANSITerminal.print_string [white] (a^"\n"));
          format_directory page selected t (counter+1))

(** [navigate_directory page d_name y lst] allows the user to navigate through
    the pages in the directory [d_name] on page [page]*)
and navigate_directory page d_name y lst =
  ANSITerminal.set_cursor 1 1;
  ANSITerminal.erase Below;
  let size = ANSITerminal.size() in 
  ANSITerminal.print_string [on_white;black] 
    ("Page " ^ (string_of_int page) ^ " of " ^ 
     (string_of_int ((List.length lst)/(snd size)+1))^ 
     "(^N for find Document/Directory, or create new document)\n");
  format_directory page y (page_list page lst) 1 ;
  Printf.printf "\027[?25l%!";
  let bytes = Bytes.create 6 in 
  ignore(Unix.read (Unix.stdin) bytes 0 6);
  match KeyPress.translate (Bytes.to_string bytes) with
  |Up -> if y = 1 then 
      if page > 1 then (navigate_directory (page-1) d_name 1 lst) 
      else navigate_directory page d_name y lst 
    else navigate_directory page d_name (y-1) lst
  |Down -> if (y = (List.length (page_list page lst)) || y = (snd size)-1) then 
      if (page * ((snd size)-1))< List.length lst then
        navigate_directory (page+1) d_name 1 lst  
      else navigate_directory page d_name y lst 
    else navigate_directory page d_name (y+1) lst
  |Nextline -> loop_file d_name (List.nth lst (((page-1)*((snd size)-1)) + y-1)) 
  |NewDoc -> create_new_file d_name 
  |_-> navigate_directory page d_name y lst 

(** [loop_file d_name f ind] is an index of [f] in [d_name]. Goes line by
    line in [f] and adds the words as keys with proper values in [ind].*)
and loop_file d_name s =
  Unix.chdir(d_name); 
  try loop_dir (Unix.opendir s) s []
  with Unix.Unix_error (_,_,_) ->
    let path = s in 
    create_state path

(** [loop_dir d d_name ind] is an index of the files of [d_name]. Goes 
    file by file starting with dir_handle [d] and adds to [ind] with each 
    loop. *)
and loop_dir d d_name lst= 
  match Unix.readdir d with
  | "." -> loop_dir d d_name lst
  | s -> loop_dir d d_name (s::lst)
  | exception End_of_file -> enter_raw_mode (); 
    navigate_directory 1 d_name 1 lst; 
    Unix.closedir d

(** [index_of_dir ()] prompts the user for a beginning directory and then loops
    through the directory*)
and index_of_dir () =
  match Stdlib.read_line () with 
  |d ->
    try loop_dir (Unix.opendir d) d []
    with Unix.Unix_error (_,_,_) -> 
      ANSITerminal.print_string [white] 
        ("Please enter the name of a valid direc\
          tory in the home or Users directory.\n");
      ANSITerminal.print_string [white] "> ";index_of_dir ()

(** [main ()] prompts the user for a starting directory and then proceeds to 
    loop through selected directory.*)
and main () = 
  ANSITerminal.print_string [white] ("Please enter the name of a valid di\
                                      rectory in the home directory (Linux), 
                                     or Users directory (Mac).\n");
  ANSITerminal.print_string [white] "> ";
  try 
    Unix.chdir (Filename.dir_sep ^ "Users");index_of_dir ()
  with _ ->
    Unix.chdir (Filename.dir_sep ^ "home");index_of_dir ()

(** starts main*)
let () = main ()