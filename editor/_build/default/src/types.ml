(* types.ml - Core data structures for the editor *)

(** Position in the text *)
type position = {
  row: int;  (* 0-indexed line number *)
  col: int;  (* 0-indexed column number *)
}

(** Direction for cursor movement *)
type direction = Up | Down | Left | Right | Home | End | PageUp | PageDown

(** Text attributes for syntax highlighting *)
type color = 
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightBlack | BrightRed | BrightGreen | BrightYellow 
  | BrightBlue | BrightMagenta | BrightCyan | BrightWhite
  | Default

type text_attribute = 
  | Bold 
  | Underline 
  | Italic
  | Reverse  (* Invert foreground and background *)
  | FgColor of color
  | BgColor of color

(** Character with associated display attributes *)
type highlighted_char = {
  chr: char;
  attrs: text_attribute list;
}

(** Token types from Task 2 language *)
type token_type = 
  | TKeyword     (* plus, minus, mult, div, cond *)
  | TIdentifier  (* variable names *)
  | TNumber      (* integer literals *)
  | TLambda      (* . in lambda expressions *)
  | TEquals      (* = in records *)
  | TComma       (* , in records *)
  | TLParen | TRParen
  | TLBrace | TRBrace     (* lazy records {} *)
  | TLBracket | TRBracket (* eager records [] *)
  | TError       (* syntax errors *)
  | TWhitespace

(** Token with position information *)
type token = {
  ttype: token_type;
  text: string;
  start_pos: position;
  end_pos: position;
}

(** Syntax analysis result *)
type syntax_analysis = {
  tokens: token list;
  errors: (string * position) list;
  brace_pairs: (position * position) list;  (* matching braces *)
}

(** Main editor state *)
type editor_state = {
  lines: string array;        (* text content, one string per line *)
  cursor: position;           (* current cursor position *)
  view_offset: position;      (* top-left corner of viewport *)
  filename: string option;    (* current file, if any *)
  modified: bool;             (* has the text been modified? *)
  syntax: syntax_analysis option;  (* cached syntax analysis *)
  selection: (position * position) option;  (* text selection, if any *)
  status_message: string;     (* message for status line *)
  window_height: int;         (* terminal height *)
  window_width: int;          (* terminal width *)
}

(** Editor commands *)
type command =
  | Insert of char
  | Delete
  | Backspace
  | Move of direction
  | Save
  | SaveAs of string
  | Open of string
  | Quit
  | ForceQuit
  | NewLine
  | Tab
  | Find of string
  | Execute  (* Run the program *)
  | Undo
  | Redo
  | Copy
  | Paste
  | Cut

(** Highlighting rules *)
type highlight_rule = {
  name: string;
  predicate: editor_state -> position -> bool;
  attributes: text_attribute list;
}

(** Configuration *)
type config = {
  tab_width: int;
  show_line_numbers: bool;
  highlight_current_line: bool;
  highlight_matching_braces: bool;
  highlight_same_identifiers: bool;
  color_scheme: (token_type * text_attribute list) list;
}

(* File mode detection *)
type file_mode = 
  | FunctionalMode  (* .func files - Task 2 language *)
  | GLProofMode     (* .gl files - GL proof checker *)
  | ReflectionMode  (* .refl files - reflection demos *)

(* GL-specific highlighting *)
type gl_token_type =
  | GLKeyword      (* Box, Lob, Axiom *)
  | GLOperator     (* →, □, ¬ *)
  | GLProof        (* proof terms *)
  | GLFormula      (* formula terms *)