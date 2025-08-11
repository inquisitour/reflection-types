(* terminal.ml - Terminal management and low-level operations *)

open Unix

(** Terminal state management *)
let original_terminal_state = ref None

(** ANSI escape codes *)
module Ansi = struct
  let esc = "\027["
  let clear_screen = esc ^ "2J"
  let clear_line = esc ^ "2K"
  let cursor_home = esc ^ "H"
  let cursor_save = esc ^ "s"
  let cursor_restore = esc ^ "u"
  let cursor_hide = esc ^ "?25l"
  let cursor_show = esc ^ "?25h"
  
  let move_cursor row col = 
    Printf.sprintf "%s%d;%dH" esc (row + 1) (col + 1)
  
  let set_color fg bg =
    let fg_code = match fg with
      | Types.Black -> 30 | Types.Red -> 31 | Types.Green -> 32 
      | Types.Yellow -> 33 | Types.Blue -> 34 | Types.Magenta -> 35
      | Types.Cyan -> 36 | Types.White -> 37
      | Types.BrightBlack -> 90 | Types.BrightRed -> 91 
      | Types.BrightGreen -> 92 | Types.BrightYellow -> 93
      | Types.BrightBlue -> 94 | Types.BrightMagenta -> 95
      | Types.BrightCyan -> 96 | Types.BrightWhite -> 97
      | Types.Default -> 39
    in
    let bg_code = match bg with
      | Types.Black -> 40 | Types.Red -> 41 | Types.Green -> 42
      | Types.Yellow -> 43 | Types.Blue -> 44 | Types.Magenta -> 45
      | Types.Cyan -> 46 | Types.White -> 47
      | Types.BrightBlack -> 100 | Types.BrightRed -> 101
      | Types.BrightGreen -> 102 | Types.BrightYellow -> 103
      | Types.BrightBlue -> 104 | Types.BrightMagenta -> 105
      | Types.BrightCyan -> 106 | Types.BrightWhite -> 107
      | Types.Default -> 49
    in
    Printf.sprintf "%s%d;%dm" esc fg_code bg_code
  
  let reset = esc ^ "0m"
  let bold = esc ^ "1m"
  let underline = esc ^ "4m"
  let italic = esc ^ "3m"
  let reverse = esc ^ "7m"
end

(** Initialize terminal for raw mode *)
let init () =
  let tattr = tcgetattr stdin in
  original_terminal_state := Some tattr;
  
  (* Set raw mode *)
  let raw_attr = {
    tattr with
    c_icanon = false;  (* Disable canonical mode *)
    c_echo = false;    (* Disable echo *)
    c_vmin = 1;        (* Minimum characters to read *)
    c_vtime = 0;       (* No timeout *)
    c_isig = false;    (* Disable signals *)
    c_ixon = false;    (* Disable software flow control *)
    c_icrnl = false;   (* Don't translate CR to NL *)
    c_opost = false;   (* Disable output processing *)
  } in
  
  tcsetattr stdin TCSAFLUSH raw_attr;
  
  (* Clear screen and hide cursor *)
  print_string Ansi.clear_screen;
  print_string Ansi.cursor_home;
  print_string Ansi.cursor_hide;
  flush Stdlib.stdout

(** Restore terminal to original state *)
let cleanup () =
  match !original_terminal_state with
  | Some tattr ->
      tcsetattr stdin TCSAFLUSH tattr;
      print_string Ansi.cursor_show;
      print_string Ansi.reset;
      print_string Ansi.clear_screen;
      print_string Ansi.cursor_home;
      flush Stdlib.stdout
  | None -> ()

(** Get terminal dimensions *)
let get_size () =
  (* Use ioctl to get terminal size *)
  try
    let (cols, rows) = ANSITerminal.size () in
    (rows, cols)
  with _ -> (24, 80)  (* Default fallback *)

(** Read a single key from input *)
let read_key () =
  let buf = Bytes.create 3 in
  let n = read stdin buf 0 3 in
  
  if n = 0 then None
  else if n = 1 then
    (* Single character *)
    let c = Bytes.get buf 0 in
    match c with
    | '\027' -> Some `Escape
    | '\n' | '\r' -> Some `Enter
    | '\127' | '\008' -> Some `Backspace  (* Both DEL and BS *)
    | '\t' -> Some `Tab
    | '\001'..'\026' -> 
        (* Control-A through Control-Z *)
        Some (`Control (Char.chr (Char.code c + 64)))
    | '\028'..'\031' -> 
        (* Other control characters *)
        Some (`Control (Char.chr (Char.code c + 64)))
    | c -> Some (`Char c)
  else if n >= 3 && Bytes.get buf 0 = '\027' && Bytes.get buf 1 = '[' then
    (* Escape sequence *)
    match Bytes.get buf 2 with
    | 'A' -> Some `Up
    | 'B' -> Some `Down
    | 'C' -> Some `Right
    | 'D' -> Some `Left
    | 'H' -> Some `Home
    | 'F' -> Some `End
    | '5' -> Some `PageUp
    | '6' -> Some `PageDown
    | _ -> Some `Unknown
  else if n = 2 && Bytes.get buf 0 = '\027' then
    (* Alt key combinations - treat as Escape for now *)
    Some `Escape
  else
    Some `Unknown

(** Move cursor to position *)
let move_cursor row col =
  print_string (Ansi.move_cursor row col);
  flush Stdlib.stdout

(** Apply text attributes *)
let set_attributes attrs =
  print_string Ansi.reset;
  List.iter (function
    | Types.Bold -> print_string Ansi.bold
    | Types.Underline -> print_string Ansi.underline
    | Types.Italic -> print_string Ansi.italic
    | Types.Reverse -> print_string Ansi.reverse
    | Types.FgColor c -> 
        print_string (Ansi.set_color c Types.Default)
    | Types.BgColor c -> 
        print_string (Ansi.set_color Types.Default c)
  ) attrs;
  flush Stdlib.stdout

(** Clear current line *)
let clear_line () =
  print_string Ansi.clear_line;
  flush Stdlib.stdout

(** Clear entire screen *)
let clear_screen () =
  print_string Ansi.clear_screen;
  print_string Ansi.cursor_home;
  flush Stdlib.stdout

(** Write string at position with attributes *)
let write_at row col str attrs =
  move_cursor row col;
  set_attributes attrs;
  print_string str;
  print_string Ansi.reset;
  flush Stdlib.stdout

(** Show cursor *)
let show_cursor () =
  print_string Ansi.cursor_show;
  flush Stdlib.stdout

(** Hide cursor *)
let hide_cursor () =
  print_string Ansi.cursor_hide;
  flush Stdlib.stdout