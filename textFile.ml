(**The type of TextFile
   -[name] is the name of the file given
   -[input] is the channel that will be used to open files for reading and 
   eventually writing *)
type t = {
  name: string;
  input: in_channel;
}

let get_name file = 
  file.name

(** [check_file_existence name] is the in_channel for [name], if it exists. If 
    it does not exist, the file is created and returns the new in_channel*)
let check_file_existence name = 
  try open_in name 
  with Sys_error s -> ignore(open_out name); open_in name 

let init_file name = {
  name = name;
  input = check_file_existence name;
}

let get_input t =
  t.input 

let write t s =
  let out = open_out t.name in 
  Printf.fprintf (out) "%s" s;
  close_out (out)