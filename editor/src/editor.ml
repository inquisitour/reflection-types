(* editor.ml - Main editor logic and state management *)

open Types

(** Create default configuration *)
let default_config () = {
  tab_width = 4;
  show_line_numbers = true;
  highlight_current_line = true;
  highlight_matching_braces = true;
  highlight_same_identifiers = true;
  color_scheme = [
    (TKeyword, [FgColor BrightBlue; Bold]);
    (TIdentifier, [FgColor White]);
    (TNumber, [FgColor Cyan]);
    (TLambda, [FgColor Magenta]);
    (TEquals, [FgColor Yellow]);
    (TComma, [FgColor White]);
    (TLParen, [FgColor BrightYellow]);
    (TRParen, [FgColor BrightYellow]);
    (TLBrace, [FgColor Green]);
    (TRBrace, [FgColor Green]);
    (TLBracket, [FgColor BrightGreen]);
    (TRBracket, [FgColor BrightGreen]);
    (TError, [FgColor Red; Underline]);
    (TWhitespace, []);
  ]
}

(** Create empty editor state *)
let create_empty () =
  let (height, width) = Terminal.get_size () in
  {
    lines = [|""|];
    cursor = { row = 0; col = 0 };
    view_offset = { row = 0; col = 0 };
    filename = None;
    modified = false;
    syntax = None;
    selection = None;
    status_message = "Ctrl-H: Help | Ctrl-S: Save | Ctrl-X: Quit | Ctrl-L: Check GL";
    window_height = height;
    window_width = width;
  }

(** Create editor state from file *)
let create_from_file filename =
  try
    let lines = File_ops.load_file filename in
    let (height, width) = Terminal.get_size () in
    {
      lines = if Array.length lines = 0 then [|""|] else lines;
      cursor = { row = 0; col = 0 };
      view_offset = { row = 0; col = 0 };
      filename = Some filename;
      modified = false;
      syntax = None;
      selection = None;
      status_message = Printf.sprintf "Loaded %s" filename;
      window_height = height;
      window_width = width;
    }
  with Failure msg ->
    let state = create_empty () in
    { state with status_message = msg }

(** Get current line *)
let current_line state =
  if state.cursor.row < Array.length state.lines then
    state.lines.(state.cursor.row)
  else
    ""

(** Get line length *)
let line_length state row =
  if row < Array.length state.lines then
    String.length state.lines.(row)
  else
    0

(** Clamp cursor position to valid range *)
let clamp_cursor state =
  let row = max 0 (min (Array.length state.lines - 1) state.cursor.row) in
  let col = max 0 (min (line_length state row) state.cursor.col) in
  { state with cursor = { row; col } }

(** Update view offset to keep cursor visible *)
let update_view_offset state =
  let view_height = state.window_height - 2 in  (* Reserve space for status *)
  let view_width = state.window_width - 6 in    (* Reserve space for line numbers *)
  
  let new_view_offset = 
    let row_offset = 
      if state.cursor.row < state.view_offset.row then
        state.cursor.row
      else if state.cursor.row >= state.view_offset.row + view_height then
        state.cursor.row - view_height + 1
      else
        state.view_offset.row
    in
    let col_offset =
      if state.cursor.col < state.view_offset.col then
        state.cursor.col
      else if state.cursor.col >= state.view_offset.col + view_width then
        state.cursor.col - view_width + 1
      else
        state.view_offset.col
    in
    { row = row_offset; col = col_offset }
  in
  { state with view_offset = new_view_offset }

(** Move cursor *)
let move_cursor state direction =
  let new_cursor = match direction with
    | Up -> 
        { state.cursor with row = max 0 (state.cursor.row - 1) }
    | Down -> 
        { state.cursor with 
          row = min (Array.length state.lines - 1) (state.cursor.row + 1) }
    | Left ->
        if state.cursor.col > 0 then
          { state.cursor with col = state.cursor.col - 1 }
        else if state.cursor.row > 0 then
          { row = state.cursor.row - 1; 
            col = line_length state (state.cursor.row - 1) }
        else
          state.cursor
    | Right ->
        let max_col = line_length state state.cursor.row in
        if state.cursor.col < max_col then
          { state.cursor with col = state.cursor.col + 1 }
        else if state.cursor.row < Array.length state.lines - 1 then
          { row = state.cursor.row + 1; col = 0 }
        else
          state.cursor
    | Home -> 
        { state.cursor with col = 0 }
    | End -> 
        { state.cursor with col = line_length state state.cursor.row }
    | PageUp ->
        { state.cursor with 
          row = max 0 (state.cursor.row - (state.window_height - 2)) }
    | PageDown ->
        { state.cursor with 
          row = min (Array.length state.lines - 1) 
                   (state.cursor.row + (state.window_height - 2)) }
  in
  { state with cursor = new_cursor } |> clamp_cursor |> update_view_offset

(** Insert character at cursor position *)
let insert_char state c =
  let line = current_line state in
  let before = String.sub line 0 state.cursor.col in
  let after = String.sub line state.cursor.col 
                (String.length line - state.cursor.col) in
  let new_line = before ^ (String.make 1 c) ^ after in
  
  let new_lines = Array.copy state.lines in
  new_lines.(state.cursor.row) <- new_line;
  
  let new_state = { state with 
    lines = new_lines;
    cursor = { state.cursor with col = state.cursor.col + 1 };
    modified = true;
    syntax = None;  (* Invalidate syntax cache *)
  } |> update_view_offset in
  
  (* Re-analyze syntax *)
  { new_state with syntax = Some (Syntax.analyze new_state.lines) }

(** Delete character at cursor position *)
let delete_char state =
  let line = current_line state in
  if state.cursor.col < String.length line then
    let before = String.sub line 0 state.cursor.col in
    let after = String.sub line (state.cursor.col + 1) 
                  (String.length line - state.cursor.col - 1) in
    let new_line = before ^ after in
    
    let new_lines = Array.copy state.lines in
    new_lines.(state.cursor.row) <- new_line;
    
    { state with 
      lines = new_lines;
      modified = true;
      syntax = None;
    }
  else if state.cursor.row < Array.length state.lines - 1 then
    (* Join with next line *)
    let next_line = state.lines.(state.cursor.row + 1) in
    let new_line = line ^ next_line in
    
    let new_lines = Array.init (Array.length state.lines - 1) (fun i ->
      if i < state.cursor.row then state.lines.(i)
      else if i = state.cursor.row then new_line
      else state.lines.(i + 1)
    ) in
    
    { state with 
      lines = new_lines;
      modified = true;
      syntax = None;
    }
  else
    state

(** Backspace - delete character before cursor *)
let backspace state =
  if state.cursor.col > 0 then
    move_cursor state Left |> delete_char
  else if state.cursor.row > 0 then
    let prev_line_len = line_length state (state.cursor.row - 1) in
    let state = move_cursor state Left in
    { (delete_char state) with 
      cursor = { row = state.cursor.row; col = prev_line_len } }
  else
    state

(** Insert new line *)
let new_line state =
  let line = current_line state in
  let before = String.sub line 0 state.cursor.col in
  let after = String.sub line state.cursor.col 
                (String.length line - state.cursor.col) in
  
  let new_lines = Array.init (Array.length state.lines + 1) (fun i ->
    if i < state.cursor.row then state.lines.(i)
    else if i = state.cursor.row then before
    else if i = state.cursor.row + 1 then after
    else state.lines.(i - 1)
  ) in
  
  { state with 
    lines = new_lines;
    cursor = { row = state.cursor.row + 1; col = 0 };
    modified = true;
    syntax = None;
  } |> update_view_offset

(** Insert tab *)
let insert_tab state config =
  let spaces = String.make config.tab_width ' ' in
  String.fold_left insert_char state spaces

(** Save file *)
let save_file state =
  match state.filename with
  | Some filename ->
      if File_ops.save_file filename state.lines then
        { state with 
          modified = false; 
          status_message = Printf.sprintf "Saved %s" filename }
      else
        { state with 
          status_message = Printf.sprintf "Error saving %s" filename }
  | None ->
      { state with status_message = "No filename set (use Ctrl-S)" }

(** Save file with new name *)
let save_file_as state filename =
  if File_ops.save_file filename state.lines then
    { state with 
      filename = Some filename;
      modified = false;
      status_message = Printf.sprintf "Saved as %s" filename }
  else
    { state with 
      status_message = Printf.sprintf "Error saving %s" filename }


(** Detect file mode from extension *)
let detect_file_mode filename =
  let ends_with suffix str =
    let len_suffix = String.length suffix in
    let len_str = String.length str in
    len_suffix <= len_str &&
    String.sub str (len_str - len_suffix) len_suffix = suffix
  in
  match filename with
  | Some f when ends_with ".gl" f -> GLProofMode
  | Some f when ends_with ".refl" f -> ReflectionMode
  | _ -> FunctionalMode

(** Get current file mode *)
let get_mode state =
  detect_file_mode state.filename

(** Check GL syntax *)
let check_gl_syntax_command state =
  let mode = get_mode state in
  if mode = GLProofMode then
    let result = Gl_syntax.check_gl_syntax state.lines in
    let message = match result with
      | Ok msg -> "GL: " ^ msg
      | Error err -> "GL Error: " ^ err
    in
    { state with status_message = message }
  else
    { state with status_message = "Not in GL mode (file must end with .gl)" }

(** Run GL demo 
let run_gl_demo state =
  let mode = get_mode state in
  if mode = GLProofMode then
    let () = Gl_syntax.run_gl_demo () in
    { state with status_message = "GL proof checked (see terminal output)" }
  else
    { state with status_message = "Not in GL mode" } *)

(** Run GL demo with output capture *)
let run_gl_demo state =
  let mode = get_mode state in
  if mode = GLProofMode then
    (* Capture the demo output *)
    let _output_buffer = Buffer.create 1024 in
    let old_stdout = Unix.dup Unix.stdout in
    let temp_file = Filename.temp_file "gl_output" ".txt" in
    let fd = Unix.openfile temp_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Unix.dup2 fd Unix.stdout;
    Unix.close fd;
    
    (* Run the demo *)
    let () = Gl_syntax.run_gl_demo () in
    
    (* Restore stdout *)
    Unix.dup2 old_stdout Unix.stdout;
    Unix.close old_stdout;
    
    (* Read the output *)
    let output = 
      try
        let ic = open_in temp_file in
        let rec read_all acc =
          try
            let line = input_line ic in
            read_all (acc ^ line ^ "\n")
          with End_of_file -> acc
        in
        let result = read_all "" in
        close_in ic;
        Sys.remove temp_file;
        result
      with _ -> "Could not capture output"
    in
    
    (* Add output as comments at the end of file *)
    let output_lines = String.split_on_char '\n' output in
    let comment_lines = List.map (fun line -> 
      if line = "" then "" else "(* " ^ line ^ " *)"
    ) output_lines in
    
    let new_lines = Array.append state.lines 
      (Array.append (Array.of_list [""; "(* GL Output: *)"]) (Array.of_list comment_lines)) in
    
    { state with 
      lines = new_lines;
      status_message = "GL proof checked - output added below";
      modified = true }
  else
    { state with status_message = "Not in GL mode" }