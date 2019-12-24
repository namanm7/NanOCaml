type t = Leaf | Node of {
    character: string;
    is_end: bool;
    hi: t;
    eq: t; 
    lo: t;
  }

let empty = 
  Leaf 

(** [insert_into_tst] is [tst] with [s] inserted into it*)
let rec insert_into_tst s tst = 
  let hd = Str.string_before s 1 in 
  let tl = Str.string_after s 1 in 
  match tst with 
  | Leaf -> Node { 
      character = hd;
      is_end = if tl = "" then true else false;
      hi = Leaf;
      eq = insert tl Leaf;
      lo = Leaf
    }
  | Node {character; is_end; hi; eq; lo} -> 
    let comparison = Stdlib.compare hd character in 
    if comparison = 0 then Node {
        character = character;
        is_end = if tl = "" then true else is_end;
        hi = hi;
        eq = insert tl eq; 
        lo = lo;
      } 
    else if comparison = 1 then Node {
        character = character;
        is_end = is_end;
        hi = insert s hi;
        eq = eq;
        lo = lo;
      } 
    else Node {
        character = character;
        is_end = is_end;
        hi = hi;
        eq = eq;
        lo = insert s lo;
      }

and insert s tst = 
  if s <> "" then insert_into_tst s tst else tst

(** [check_or_convert_word str] returns the word given by [str], if there 
          is a word in [str]. The word is also converted to lowercase
          Requires: no more than one word in [str]*)
let check_or_convert_word str = 
  let regex = Str.regexp "[A-Z|a-z|0-9]" in 
  try 
    let match_forward_string = 
      Str.search_forward regex str 0 |> Str.string_after str in 
    let str_len = String.length match_forward_string in
    let match_backward_index = 
      Str.search_backward regex match_forward_string (str_len - 1) in 
    if match_backward_index = (str_len - 1) 
    then match_forward_string |> String.lowercase_ascii
    else Str.string_before match_forward_string (match_backward_index +1) 
         |> String.lowercase_ascii
  with Not_found -> str

(** [search_tst s tst] is true if [s] is in tst and false otherwise.*)
let rec search_tst s tst = 
  try 
    let hd = Str.string_before s 1 in 
    let tl = Str.string_after s 1 in 
    match tst with
    | Leaf -> false
    | Node {character; is_end; hi; eq; lo} -> 
      let comparison = Stdlib.compare hd character in 
      if comparison = 0 then 
        if tl = "" then is_end
        else search_tst tl eq
      else if comparison = 1 then search_tst s hi 
      else search_tst s lo 
  with _ -> false 

(** [search_special_tst lst tst] is true if each element of [lst] is in [tst] 
    and is false otherwise*)
let rec search_special_tst lst tst = 
  match lst with 
  | [] -> true 
  | h::t -> if search_tst(check_or_convert_word h) tst 
    then search_special_tst t tst 
    else false

let search s tst = 
  if s <> "" then 
    (let extract  = check_or_convert_word s in 
     if String.contains extract '-' then 
       search_special_tst (String.split_on_char '-' extract) tst 
     else if String.contains extract '\'' then 
       search_special_tst (String.split_on_char '\'' extract) tst 
     else search_tst extract tst )
  else true 
