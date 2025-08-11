(* file_ops.ml - File operations for loading and saving *)

(** Read file into array of lines *)
let load_file filename =
  try
    let ic = open_in filename in
    let lines = ref [] in
    try
      while true do
        lines := input_line ic :: !lines
      done;
      [||]  (* Never reached *)
    with End_of_file ->
      close_in ic;
      !lines |> List.rev |> Array.of_list
  with
  | Sys_error msg -> 
      failwith (Printf.sprintf "Cannot open file %s: %s" filename msg)
  | _ -> 
      failwith (Printf.sprintf "Error reading file %s" filename)

(** Save lines to file *)
let save_file filename lines =
  try
    let oc = open_out filename in
    Array.iter (fun line ->
      output_string oc line;
      output_char oc '\n'
    ) lines;
    close_out oc;
    true
  with
  | Sys_error msg ->
      Printf.eprintf "Cannot save file %s: %s\n" filename msg;
      false
  | _ ->
      Printf.eprintf "Error saving file %s\n" filename;
      false

(** Check if file exists *)
let file_exists filename =
  Sys.file_exists filename

(** Get file extension *)
let get_extension filename =
  try
    let dot_idx = String.rindex filename '.' in
    String.sub filename (dot_idx + 1) (String.length filename - dot_idx - 1)
  with Not_found -> ""

(** Create backup file *)
let create_backup filename =
  if file_exists filename then
    let backup_name = filename ^ ".bak" in
    try
      let lines = load_file filename in
      save_file backup_name lines
    with _ -> false
  else
    true

(** Create empty file if it doesn't exist *)
let ensure_file_exists filename =
  if not (file_exists filename) then
    save_file filename [||]
  else
    true

(** Get file info *)
type file_info = {
  name: string;
  size: int;
  modified_time: float;
  is_readonly: bool;
}

let get_file_info filename =
  try
    let stats = Unix.stat filename in
    Some {
      name = filename;
      size = stats.st_size;
      modified_time = stats.st_mtime;
      is_readonly = not (Unix.access filename [Unix.W_OK]; true);
    }
  with _ -> None

(** Read from stdin *)
let read_from_stdin () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done;
    [||]
  with End_of_file ->
    !lines |> List.rev |> Array.of_list

(** Write to stdout *)
let write_to_stdout lines =
  Array.iter (fun line ->
    print_endline line
  ) lines