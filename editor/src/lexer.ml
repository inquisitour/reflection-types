(* lexer.ml - Lexer for the functional language from Task 2 *)

open Types

(** Check if character is whitespace *)
let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

(** Check if character is digit *)
let is_digit = function
  | '0'..'9' -> true
  | _ -> false

(** Check if character is letter *)
let is_letter = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

(** Check if character is valid in identifier *)
let is_ident_char c =
  is_letter c || is_digit c || c = '_'

(** Keywords in the language *)
let keywords = ["cond"; "plus"; "minus"; "mult"; "div"; "add"; "reduce"]

(** Check if string is a keyword *)
let is_keyword s =
  List.mem s keywords

(** Lex a single token from string starting at position *)
let lex_token str pos line col =
  if pos >= String.length str then
    None
  else
    let c = String.get str pos in
    match c with
    | ' ' | '\t' ->
        (* Whitespace *)
        let rec skip_space p c =
          if p >= String.length str then (p, c)
          else match String.get str p with
            | ' ' -> skip_space (p + 1) (c + 1)
            | '\t' -> skip_space (p + 1) (c + 4)  (* Tab counts as 4 spaces *)
            | _ -> (p, c)
        in
        let (end_pos, end_col) = skip_space (pos + 1) (col + 1) in
        Some ({
          ttype = TWhitespace;
          text = String.sub str pos (end_pos - pos);
          start_pos = { row = line; col };
          end_pos = { row = line; col = end_col };
        }, end_pos, line, end_col)
    
    | '\n' ->
        (* Newline *)
        Some ({
          ttype = TWhitespace;
          text = "\n";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line + 1, 0)
    
    | '(' -> 
        Some ({
          ttype = TLParen;
          text = "(";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | ')' -> 
        Some ({
          ttype = TRParen;
          text = ")";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | '{' -> 
        Some ({
          ttype = TLBrace;
          text = "{";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | '}' -> 
        Some ({
          ttype = TRBrace;
          text = "}";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | '[' -> 
        Some ({
          ttype = TLBracket;
          text = "[";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | ']' -> 
        Some ({
          ttype = TRBracket;
          text = "]";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | '.' -> 
        (* Lambda dot or error *)
        Some ({
          ttype = TLambda;
          text = ".";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | '=' -> 
        Some ({
          ttype = TEquals;
          text = "=";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | ',' -> 
        Some ({
          ttype = TComma;
          text = ",";
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)
    
    | c when is_digit c ->
        (* Number *)
        let rec read_number p =
          if p >= String.length str then p
          else if is_digit (String.get str p) then read_number (p + 1)
          else p
        in
        let end_pos = read_number (pos + 1) in
        let text = String.sub str pos (end_pos - pos) in
        Some ({
          ttype = TNumber;
          text;
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + String.length text };
        }, end_pos, line, col + String.length text)
    
    | c when is_letter c ->
        (* Identifier or keyword *)
        let rec read_ident p =
          if p >= String.length str then p
          else if is_ident_char (String.get str p) then read_ident (p + 1)
          else p
        in
        let end_pos = read_ident (pos + 1) in
        let text = String.sub str pos (end_pos - pos) in
        let ttype = if is_keyword text then TKeyword else TIdentifier in
        Some ({
          ttype;
          text;
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + String.length text };
        }, end_pos, line, col + String.length text)
    
    | _ ->
        (* Unknown character - treat as error *)
        Some ({
          ttype = TError;
          text = String.make 1 c;
          start_pos = { row = line; col };
          end_pos = { row = line; col = col + 1 };
        }, pos + 1, line, col + 1)

(** Lex entire string into tokens *)
let lex_string str =
  let rec lex_all pos line col acc =
    match lex_token str pos line col with
    | None -> List.rev acc
    | Some (token, new_pos, new_line, new_col) ->
        lex_all new_pos new_line new_col (token :: acc)
  in
  lex_all 0 0 0 []

(** Lex array of lines *)
let lex_lines lines =
  let full_text = String.concat "\n" (Array.to_list lines) in
  lex_string full_text

(** Find matching braces in token list *)
let find_brace_pairs tokens =
  let rec find_pairs tokens stack pairs =
    match tokens with
    | [] -> pairs
    | tok :: rest ->
        match tok.ttype with
        | TLParen | TLBrace | TLBracket ->
            find_pairs rest (tok :: stack) pairs
        | TRParen ->
            (match stack with
             | open_tok :: stack_rest when open_tok.ttype = TLParen ->
                 find_pairs rest stack_rest 
                   ((open_tok.start_pos, tok.start_pos) :: pairs)
             | _ -> find_pairs rest stack pairs)  (* Unmatched *)
        | TRBrace ->
            (match stack with
             | open_tok :: stack_rest when open_tok.ttype = TLBrace ->
                 find_pairs rest stack_rest 
                   ((open_tok.start_pos, tok.start_pos) :: pairs)
             | _ -> find_pairs rest stack pairs)
        | TRBracket ->
            (match stack with
             | open_tok :: stack_rest when open_tok.ttype = TLBracket ->
                 find_pairs rest stack_rest 
                   ((open_tok.start_pos, tok.start_pos) :: pairs)
             | _ -> find_pairs rest stack pairs)
        | _ -> find_pairs rest stack pairs
  in
  find_pairs tokens [] []

(** Find syntax errors (unbalanced braces) *)
let find_errors tokens =
  let rec check_balance tokens stack errors =
    match tokens with
    | [] -> 
        (* Any remaining open braces are errors *)
        List.fold_left (fun errs tok ->
          ("Unmatched opening brace", tok.start_pos) :: errs
        ) errors stack
    | tok :: rest ->
        match tok.ttype with
        | TLParen | TLBrace | TLBracket ->
            check_balance rest (tok :: stack) errors
        | TRParen ->
            (match stack with
             | open_tok :: stack_rest when open_tok.ttype = TLParen ->
                 check_balance rest stack_rest errors
             | _ -> 
                 check_balance rest stack 
                   (("Unmatched closing parenthesis", tok.start_pos) :: errors))
        | TRBrace ->
            (match stack with
             | open_tok :: stack_rest when open_tok.ttype = TLBrace ->
                 check_balance rest stack_rest errors
             | _ -> 
                 check_balance rest stack 
                   (("Unmatched closing brace", tok.start_pos) :: errors))
        | TRBracket ->
            (match stack with
             | open_tok :: stack_rest when open_tok.ttype = TLBracket ->
                 check_balance rest stack_rest errors
             | _ -> 
                 check_balance rest stack 
                   (("Unmatched closing bracket", tok.start_pos) :: errors))
        | _ -> check_balance rest stack errors
  in
  List.rev (check_balance tokens [] [])