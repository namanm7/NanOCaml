(**The type of a state of the NanOCaml editor. 
   -[number] keeps track of the specific iteration of the state, starting 
   from 0 at initilization 
   -[buffer] holds all the text read from a while, including any changes 
   that have been made during the NanOCaml session
   -[textfile] is TextFile that will eventually be used to write
   -[length] is the current length in terms of line of the file 
   -[voff] is the vertical offset that will be used to determine 
   how much to pan the screen to the bottom
   -[hoff] is the horizotnal offset that will be used to determine
   how much to pan the screen to the right
   -[cursor] is the current location of the cursor in the terminal
   -[portals] is a list of the current portals that the user has placed *)
type t = {
  number: int;
  buffer: string array;
  textfile: TextFile.t;
  length: int;
  voff: int;
  hoff: int;
  cursor: int * int;
  portals: int list;
}

(** [clipboard] is the clipboard of NanOCaml, holing what was last copied or
    pasted, if anything*)
let clipboard = ref (Array.make 1 "")

let undoStack = Stack.create ()

let redoStack = Stack.create ()

let get_voff st =
  st.voff

let get_textfile st =
  st.textfile

let get_hoff st = 
  st.hoff

let get_cursor_pos st =
  st.cursor 

let get_length st =
  st.length

let get_portals st = 
  st.portals

(** [resize_array arry] is an array of double length of [arry] with empty 
    strings as new entries*)
let resize_array arry = 
  let curr_length = Array.length arry in
  let new_arry = Array.make curr_length ("") in
  Array.append arry new_arry

(** [add_to_array arry line value] is [arry] with value in line number [line].
    If [arry] is too small, it is resized to double its current length*)
let add_to_array arry line value =
  try 
    Array.set arry line value; arry
  with _ -> 
    let new_arry = resize_array arry in 
    Array.set (new_arry) line value;
    new_arry

let rec read_from_file ic bufferarry line = 
  try
    match (input_line ic) with 
    | s -> 
      let buff = "" in 
      let added = buff^s in 
      read_from_file ic (add_to_array bufferarry line added) (line+1)
  with End_of_file -> close_in ic; (bufferarry,line)

(** [remove_portal line lst acc] is [lst] without [line]*)
let rec remove_portal line lst acc =
  match lst with 
  | [] -> acc 
  | h::t -> if h = line then acc@t else remove_portal line t (h::acc)

(** [shift_portals st dir] is the portals of [st] shifted in the direction given 
    by [dir] if they are above or below the current line, depending on [dir] *)
let shift_portals st dir = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let current_line = (y-2)+voff in 
  let portals = get_portals st in 
  List.map (fun x -> if x >= current_line then x+(dir) else x) portals

(** [shift_elements_down st arry from] shifts down the lines of [arry] from line 
    number [from]*)
let rec shift_elements_down st arry from = 
  let current_end = st.length in 
  let answer = ref arry in 
  for i = (current_end) downto (from+1) do
    answer := add_to_array arry i (Array.get arry (i-1))
  done; !answer

(** [shift_elements_up st arry from] shifts up the lines of [arry] from line 
    number [from]*)
let shift_elements_up st arry from = 
  let current_end = st.length in 
  for i = from to (current_end-2) do (**CHECK *)
    Array.set arry i (Array.get arry (i+1))
  done; () 

let copy st = 
  {st with buffer = Array.copy (st.buffer)}

let init_state txtfile = 
  let content,size = read_from_file (TextFile.get_input txtfile) 
      ("" |> Array.make 500 ) 0 in 
  let verified_size = (if size <> 0 then size else 
                         (Array.set content 0 (""); size+1)) in 
  let return_state = {
    number = 0;
    buffer = content;
    textfile = txtfile;
    length = verified_size;
    voff = 0;
    hoff = 0; 
    cursor = (7,2);
    portals = []
  } in 
  Stack.push (copy return_state) undoStack; return_state

let help_state txtfile =
  let content,size = read_from_file (TextFile.get_input txtfile) 
      ("" |> Array.make 500 ) 0 in 
  let verified_size = (if size <> 0 then size else 
                         (Array.set content 0 (""); size+1)) in 
  let return_state = {
    number = 0;
    buffer = content;
    textfile = txtfile;
    length = verified_size;
    voff = 0;
    hoff = 0; 
    cursor = (7,2);
    portals = []
  } in 
  return_state

(** [create_new_buffer_array_with_s st s row col] is a tuple containing a new 
    array with [s] placed in row [row] and column [col] in [st], and the new 
    length*)
let create_new_buffer_array_with_s st s row col = 
  let arry = st.buffer in 
  let buff = try 
      Array.get arry row 
    with _ -> "" in 
  let contents = buff in 
  let part1 = Str.string_before contents col in 
  let part2 = Str.string_after contents col in 
  let new_buff = "" in 
  match s with
  | "\n" -> let new_buff2 = "" in 
    let added_new_buff = new_buff^part1 in 
    let added_new_buff_2 = new_buff2^part2 in 
    let new_arry = shift_elements_down st arry row in 
    Array.set new_arry row added_new_buff;
    Array.set new_arry (row+1) added_new_buff_2;
    (new_arry,1)
  | _ ->
    let added_part1 = new_buff^part1 in 
    let added_s = added_part1^s in 
    let added_part2 = added_s^part2 in 
    Array.set arry row added_part2;
    (arry,0)

let add_string st s row col = 
  let contents,change = create_new_buffer_array_with_s st s row col in 
  {st with number = st.number + 1; 
           buffer = contents; 
           length = st.length + change;}

(** [create_new_buffer_remove_char st row col] is a tuple containing a new array
    with the chracter at [row] [col] deleted in [st], and the new length*)
let create_new_buffer_remove_char st row col =
  let arry = st.buffer in 
  let buff = try 
      Array.get arry row  
    with _ -> "" in   
  let contents = buff in 
  if (col = 0 && row = 0) then (arry,0)
  else if (col = 0&&row>0) then 
    begin 
      let added_contents = (Array.get arry (row-1))^contents in 
      Array.set arry (row-1) added_contents; 
      shift_elements_up st arry row;
      (arry,(-1))
    end 
  else
    let part1 = Str.string_before contents (col - 1) in 
    let part2 = Str.string_after contents col in 
    let new_buff = "" in 
    let added_part1 = new_buff^part1 in 
    let added_part2 = added_part1^part2 in 
    Array.set arry row added_part2;
    (arry,0)

let remove_char st row col = 
  let contents,change = create_new_buffer_remove_char st row col in 
  {st with  
   number = st.number + 1;
   buffer = contents;
   length = st.length + change;}

(** [buffer_arry_to_string length acc arry] is the contents of 
    [arry] concatenated together with '\n' in between lines*)
let rec buffer_arry_to_string length acc arry =
  let ref_acc = ref acc in 
  for i = 0 to (length-1) do 
    let row_contents = (Array.get arry i) in 
    ref_acc := !ref_acc^row_contents;
    ref_acc := !ref_acc ^ "\n";
    Array.set arry i !ref_acc;
  done;
  let last_row_contents = Array.get arry length in 
  ref_acc := !ref_acc^last_row_contents;
  !ref_acc

let current_text st = 
  let st = st.buffer |> buffer_arry_to_string st.length ("") in st

let current_text_lines st =
  Array.sub st.buffer 0 st.length 

let write st = 
  TextFile.write (st.textfile) (current_text st)

(** [find_and_replace_string find replace string] searches [string] for [find]
    and replaces all instances of [find] in [string] with [replace]*)
let find_and_replace_string find replace string = 
  let reg_ex = Str.regexp_string_case_fold find in 
  Str.global_replace reg_ex replace string

(** [find_and_replace_row_by_row find replace arry length] is [arry] with all 
    instances of [find] replaced with [replace]*)
let find_and_replace_row_by_row find replace arry length = 
  for i = 0 to length do 
    let buff = Array.get arry i in 
    let content = buff in 
    let new_content = find_and_replace_string find replace content in 
    Array.set arry i new_content;
  done;
  arry

let find_and_replace st find replace = 
  if find = "" then st else 
    let return_state = 
      {st with number = st.number + 1;
               buffer = find_and_replace_row_by_row find 
                   replace st.buffer st.length;
               cursor = (7,2);} in 
    Stack.push (copy return_state) undoStack; return_state

(** [find_eol length st] is the point giving the location of the end of line in
    [st]*)
let find_eol length st =
  let width, height = ANSITerminal.size() in 
  if (length + 6) < width then 
    (length+6+1,0)
  else 
    let multiple = (length)/(width-6) in 
    let overflow = length mod (width-6) in 
    let adjust = 1 + overflow + (multiple-1)*(width-6) in
    (width, adjust)

(** [move_cursor_vertical curr_col destination st] is the point giving the 
    location of the cursor after an attempt to move it vertically to 
    [destination]*)
let move_cursor_vertical curr_col destination st =
  let hoff = get_hoff st in 
  let destination_length = String.length destination in 
  if (curr_col + hoff) <= (destination_length+6) then (curr_col, hoff)
  else find_eol destination_length st 

let move_cursor_up st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let array = st.buffer in 
  if y = 2 then 
    if voff = 0 then st 
    else 
      let new_col, new_hoff = 
        move_cursor_vertical x (Array.get array (y-3+voff)) st in 
      (ANSITerminal.move_bol(); ANSITerminal.erase Below;
       {st with voff = st.voff - 1;
                hoff = new_hoff;
                cursor = (new_col,y)})
  else 
    let new_col, new_hoff = 
      move_cursor_vertical x (Array.get array (y-3+voff)) st in 
    {st with hoff = new_hoff;
             cursor = (new_col,y-1)}

let move_cursor_down st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let text = current_text_lines st in 
  let size = ANSITerminal.size() in 
  if y = snd size 
  then (ANSITerminal.set_cursor 1 2; 
        ANSITerminal.erase Below; if voff - 1 + snd size = (Array.length text) 
        then st else 
          let new_col, new_hoff = 
            move_cursor_vertical x (Array.get text (y-1+voff)) st in 
          {st with voff = st.voff + 1;
                   hoff = new_hoff;
                   cursor = (new_col,y);})
  else if y-2+voff = ((Array.length text)-1) then st 
  else 
    let new_col, new_hoff = 
      move_cursor_vertical x (Array.get text (y-1+voff)) st in 
    {st with hoff = new_hoff;
             cursor = (new_col,y+1);}

(** [pan_screen_right current_line curr_col hoff] is the new horizontal offset
    if the screen pans right*)
let pan_screen_right current_line curr_col hoff = 
  let line_length = 7+String.length current_line in 
  if curr_col + hoff < line_length then hoff+1 else hoff

(** [move_cursor_to_right curr_col currentLine hoff] is the point location of 
    the cursor whent the user presses the right arrow. If at the end of the line
    the point location of the cursor remains unchanged *)
let move_cursor_to_right curr_col currentLine hoff = 
  let line_length = String.length currentLine in 
  if curr_col+hoff >= (line_length+7) then curr_col else curr_col+1

let move_cursor_right st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let hoff = get_hoff st in 
  let text = current_text_lines st in 
  let size = ANSITerminal.size() in 
  if x = fst size then 
    let new_hoff = pan_screen_right (Array.get text (y-2+voff)) x hoff in 
    {st with hoff = new_hoff;}
  else
    let new_col = move_cursor_to_right x (Array.get text (y-2+voff)) hoff in 
    {st with cursor = (new_col,y);}

let move_cursor_left st = 
  let x,y = get_cursor_pos st in 
  let hoff = get_hoff st in 
  if x = 7 then 
    let new_hoff = if hoff > 0 then hoff-1 else hoff in 
    {st with hoff = new_hoff;}
  else 
    {st with cursor = (x-1,y);}

let add_nextline st = 
  let x,y = get_cursor_pos st in 
  let size = ANSITerminal.size() in 
  let voff = get_voff st in 
  let hoff = get_hoff st in 
  let new_st = add_string st "\n" (y-2+voff) (x-7+hoff) in 
  let new_portals = shift_portals st 1 in 
  ANSITerminal.move_bol(); ANSITerminal.erase Below; 
  if y = (snd size) then 
    (ANSITerminal.set_cursor 1 2; 
     ANSITerminal.erase Below; 
     let return_state = {new_st with voff = new_st.voff + 1;
                                     hoff = 0;
                                     cursor = (7,y);
                                     portals = new_portals} in 
     Stack.push (copy return_state) undoStack; return_state)
  else 
    let return_state = {new_st with hoff = 0;
                                    cursor = (7,y+1);
                                    portals = new_portals} in 
    Stack.push (copy return_state) undoStack; return_state

let add_tab st = 
  let x,y = get_cursor_pos st in 
  let size = ANSITerminal.size() in 
  let voff = get_voff st in 
  let hoff = get_hoff st in 
  let new_st = add_string st "     " (y-2+voff) (x-7+hoff) in 
  let new_col = if x = (fst size) then x else x+5 in 
  let new_hoff = if x = (fst size) then hoff + 5 else hoff in 
  let return_state = {new_st with hoff = new_hoff;
                                  cursor = (new_col,y);} in 
  Stack.push (copy return_state) undoStack; return_state

let add_char st s =
  let x,y = get_cursor_pos st in 
  let size = ANSITerminal.size() in 
  let voff = get_voff st in 
  let hoff = get_hoff st in 
  let new_st = add_string st s (y-2+voff) (x-7+hoff) in 
  let new_col = if x = (fst size) then x else x+1 in 
  let new_hoff = if x = (fst size) then hoff + 1 else hoff in 
  let return_state = {new_st with cursor = (new_col,y); 
                                  hoff = new_hoff;} in 
  Stack.push (copy return_state) undoStack; return_state

let handle_backspace st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let hoff = get_hoff st in 
  let text = current_text_lines st in 
  let new_st = remove_char st (y-2+voff) (x-7+hoff) in 
  if x = 7 && y > 2 && hoff = 0 then 
    let curr_row = y-2+voff in 
    let target_length = String.length (Array.get text (curr_row-1)) in 
    let new_col, new_hoff = find_eol target_length st in 
    let new_portals = shift_portals st (-1) in 
    let return_state = {new_st with hoff = new_hoff;
                                    cursor = (new_col,y-1);
                                    portals = new_portals} in 
    Stack.push (copy return_state) undoStack; ANSITerminal.set_cursor 7 2; 
    ANSITerminal.erase Below; return_state
  else if x = 7 && y > 2 && hoff > 0 then 
    let return_state = {new_st with hoff = new_st.hoff - 1;} in 
    Stack.push (copy return_state) undoStack;   ANSITerminal.set_cursor 7 2; 
    ANSITerminal.erase Below; return_state
  else if x = 7 && y = 2 && hoff = 0 then 
    (if voff > 0 then 
       let curr_row = y-2+voff in 
       let target_length = String.length (Array.get text (curr_row-1)) in 
       let new_col, new_hoff = find_eol target_length st in 
       let new_portals = shift_portals st (-1) in 
       let return_state = {new_st with voff = new_st.voff - 1;
                                       hoff = new_hoff;
                                       cursor = (new_col,y);
                                       portals = new_portals} in 
       Stack.push (copy return_state) undoStack; 
       ANSITerminal.set_cursor 7 2; 
       ANSITerminal.erase Below; return_state 
     else (Stack.push (copy st) undoStack; st)) 
  else if x = 7 && y = 2 && hoff > 0 then 
    let return_state = {new_st with hoff = new_st.hoff - 1;} in 
    Stack.push (copy return_state) undoStack; ANSITerminal.set_cursor 7 2; 
    ANSITerminal.erase Below; return_state
  else 
    let return_state = {new_st with cursor = (x-1,y);} in 
    Stack.push (copy return_state) undoStack;
    return_state 

let handle_undo st = 
  ANSITerminal.set_cursor 1 2; ANSITerminal.erase Below;
  if Stack.length undoStack <> 1 then 
    (Stack.push (Stack.pop undoStack) redoStack; 
     copy(Stack.top undoStack)) else st

let handle_redo st = 
  ANSITerminal.set_cursor 1 2;ANSITerminal.erase Below;
  if Stack.length redoStack <>0 
  then (Stack.push (Stack.pop redoStack) undoStack; copy (Stack.top undoStack)) 
  else st

let clear_redo () = 
  Stack.clear redoStack; ()

let move_bol st = 
  let x,y = get_cursor_pos st in 
  let hoff = get_hoff st in 
  if x = 7 && hoff = 0 then st 
  else 
    {st with cursor = (7,y); 
             hoff = 0}

let move_eol st = 
  ANSITerminal.set_cursor 7 2; 
  ANSITerminal.erase Below;
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let hoff = get_hoff st in
  let line = Array.get st.buffer (y+voff-2) in 
  let length = String.length line in 
  let width, height = ANSITerminal.size() in 
  if (length + 6 + 1) < width then 
    (if x <> length+6+1 then 
       {st with cursor = (length+6+1,y)}
     else st) 
  else 
    let multiple = (length)/(width-6) in 
    let overflow = length mod (width-6) in 
    let adjust = 1 + overflow + (multiple-1)*(width-6) in
    if x=width && hoff=adjust then st else 
      {st with cursor = (width,y); 
               hoff = adjust}

let move_top st = 
  ANSITerminal.set_cursor 7 2; 
  ANSITerminal.erase Below;
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  if y = 2 && voff = 0 then st 
  else 
    {st with cursor = (7,2); 
             voff = 0; 
             hoff = 0}

let move_btm st = 
  let x,y = get_cursor_pos st in 
  let width, height = ANSITerminal.size() in 
  let voff = get_voff st in 
  let line_count = get_length st in 
  let multiple = line_count/(height-1) in 
  let overflow = line_count mod (height-1) in 
  let adjust = overflow + (multiple-1)*(height-1) in 
  if  (line_count + 1) <= height then 
    (if y <> line_count+1 then
       {st with hoff = 0; cursor = (7,line_count+1)}
     else st) 
  else 
  if y = height && voff = adjust then st else 
    let return_state = {st with cursor = (7,height); 
                                voff = adjust; hoff = 0} in 
    ANSITerminal.set_cursor 7 2; 
    ANSITerminal.erase Below; return_state

let jump st target_line = 
  let size = get_length st in 
  if target_line > size then (ANSITerminal.set_cursor 7 2; 
                              ANSITerminal.erase Below; st) 
  else 
    let x,y = get_cursor_pos st in 
    let width, height = ANSITerminal.size() in 
    let voff = get_voff st in 
    let multiple = target_line/(height-1) in 
    let overflow = target_line mod (height-1) in 
    let adjust = overflow + (multiple-1)*(height-1) in 
    if  (target_line + 1) <= height then 
      (if y <> target_line+1 then
         let return_state = {st with voff=0; 
                                     hoff = 0; 
                                     cursor = (7,target_line+1)} in 
         ANSITerminal.set_cursor 7 2; 
         ANSITerminal.erase Below; return_state
       else (ANSITerminal.set_cursor 7 2; 
             ANSITerminal.erase Below; st) ) 
    else 
    if y = height && voff = adjust then st else 
      let return_state = {st with cursor = (7,height); 
                                  voff = adjust; hoff = 0} in 
      ANSITerminal.set_cursor 7 2; 
      ANSITerminal.erase Below; return_state

let toggle_portal st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let current_line = (y-2)+voff in 
  let portals = get_portals st in 
  if List.mem current_line portals then 
    let new_portals = remove_portal current_line portals [] in 
    let return_state = {st with number = st.number + 1; 
                                portals = new_portals} in 
    Stack.push (copy return_state) undoStack; return_state
  else 
    let new_portals = current_line::portals in
    let return_state = {st with number = st.number + 1; 
                                portals = new_portals} in 
    Stack.push (copy return_state) undoStack; return_state

(** [return_max lst] is the max value of [lst]*)
let return_max lst = 
  let rev_sorted = List.rev (List.sort compare lst) in 
  match rev_sorted with 
  | h::t -> h
  | _ -> 0 (*Should never happen *)

(** [return_min lst] is the minimum value of [lst]*)
let return_min lst = 
  let sorted = List.sort compare lst in 
  match sorted with 
  | h::t -> h
  | _ -> 0 (*Should never happen *)

let portal_jump_up st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let current_line = (y-2)+voff in 
  let portals = get_portals st in 
  let above = List.filter (fun x -> x < current_line) portals in 
  if List.length above = 0 then st else 
    let max = return_max above in 
    jump st (max+1)

let portal_jump_down st = 
  let x,y = get_cursor_pos st in 
  let voff = get_voff st in 
  let current_line = (y-2)+voff in 
  let portals = get_portals st in 
  let below = List.filter (fun x -> x > current_line) portals in 
  if List.length below = 0 then st else 
    let min = return_min below in 
    jump st (min+1)

(** [remove_lines st start stop] removes the lines in [st] from [start] to 
    [stop], inclusive*)
let remove_lines st start stop =
  let arry = st.buffer in 
  let num_lines_deleted = stop-start+1 in 
  let new_length = (get_length st) - num_lines_deleted in 
  let content_removed = Array.sub arry (start-1) num_lines_deleted in 
  let portals = ref (get_portals st) in 
  clipboard := content_removed; 
  if new_length >= start then 
    for i = (start-1) to (new_length) do 
      let target_line = i + num_lines_deleted in 
      Array.set arry i (Array.get arry (target_line))
    done; 
  portals := List.filter (fun x -> x < (start-1) || x > (stop-1)) !portals;
  portals := List.map (fun x -> if x > (stop-1) 
                        then (x-num_lines_deleted) else x) !portals;
  (new_length, !portals)

let cut st (start,stop) = 
  if start = 0 || stop = 0 
  then (ANSITerminal.set_cursor 7 2; ANSITerminal.erase Below; st)
  else 
    let size = get_length st in 
    if stop > size && stop < start then st else 
      let new_length,new_portals = remove_lines st start stop in 
      let lines_removed = {st with number = st.number + 1; 
                                   length = new_length;
                                   portals = new_portals} in 
      let return_state = jump lines_removed start in 
      Stack.push (copy return_state) undoStack; return_state

let handle_copy st (start,stop) = 
  if start = 0 || stop = 0 
  then (ANSITerminal.set_cursor 7 2; ANSITerminal.erase Below; st)
  else 
    let size = get_length st in 
    let arry = st.buffer in
    if stop > size && stop < start then st else 
      let copied_content = Array.sub arry (start-1) (stop-start+1) in 
      clipboard := copied_content; st 

(** [paste_in_lines st target target_size] is the triple consisting of the new
    length of the buffer in [st] once the clipboard contents are pasted in line 
    [target], the list of portals once the clipboard contents are pasted, and
    the buffer containing the pasted contents embedded in the buffer belonging
    to [st]*)
let paste_in_lines st target target_size =
  let arry = st.buffer in 
  let new_length = (get_length st) + target_size in  
  let portals = ref (get_portals st) in 
  let answer = Array.make (target_size) "" |> Array.append arry in
  for i = (new_length-1) downto (target + target_size - 1) do
    Array.set answer i (Array.get arry (i-target_size))
  done; 
  for i = (target - 1) to (target + target_size -2) do 
    let target_line = i - (target - 1) in 
    Array.set answer i (Array.get !clipboard (target_line))
  done; 
  portals := List.map (fun x -> if x >= (target-1) then (x+target_size) else x) 
      !portals;
  (new_length, !portals, answer)


let paste st target = 
  let size = get_length st in
  let target_size = Array.length !clipboard in 
  if target > 0 && target <= size then 
    let new_length,new_portals,new_array = 
      paste_in_lines st target target_size in 
    let lines_added = {st with number = st.number + 1; 
                               length = new_length;
                               portals = new_portals;
                               buffer = new_array} in
    ANSITerminal.set_cursor 7 2; 
    ANSITerminal.erase Below; 
    Stack.push (copy lines_added) undoStack; lines_added
  else (ANSITerminal.set_cursor 7 2; ANSITerminal.erase Below; st)
