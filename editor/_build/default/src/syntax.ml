(* syntax.ml - Syntax analysis for the functional language *)

open Types

(** Analyze syntax of editor lines *)
let analyze lines =
  (* Lex the entire text *)
  let tokens = Lexer.lex_lines lines in
  
  (* Find matching braces *)
  let brace_pairs = Lexer.find_brace_pairs tokens in
  
  (* Find syntax errors *)
  let errors = Lexer.find_errors tokens in
  
  {
    tokens;
    errors;
    brace_pairs;
  }

(** Get token at position *)
let token_at_position syntax pos =
  List.find_opt (fun tok ->
    tok.start_pos.row = pos.row &&
    tok.start_pos.col <= pos.col &&
    tok.end_pos.col > pos.col
  ) syntax.tokens

(** Get all tokens with same text as token at position *)
let same_identifier_tokens syntax pos =
  match token_at_position syntax pos with
  | Some tok when tok.ttype = TIdentifier ->
      List.filter (fun t ->
        t.ttype = TIdentifier && t.text = tok.text
      ) syntax.tokens
  | _ -> []

(** Find matching brace for position *)
let matching_brace syntax pos =
  (* Check if we're on an opening brace *)
  let open_match = List.find_opt (fun (open_pos, _) ->
    open_pos.row = pos.row && open_pos.col = pos.col
  ) syntax.brace_pairs in
  
  (* Check if we're on a closing brace *)
  let close_match = List.find_opt (fun (_, close_pos) ->
    close_pos.row = pos.row && close_pos.col = pos.col
  ) syntax.brace_pairs in
  
  match open_match, close_match with
  | Some (_, close_pos), _ -> Some close_pos
  | _, Some (open_pos, _) -> Some open_pos
  | None, None -> None

(** Check if position has a syntax error *)
let has_error_at syntax pos =
  List.exists (fun (_, err_pos) ->
    err_pos.row = pos.row && err_pos.col = pos.col
  ) syntax.errors

(** Get error message at position *)
let error_at syntax pos =
  List.find_opt (fun (_, err_pos) ->
    err_pos.row = pos.row && err_pos.col = pos.col
  ) syntax.errors |> Option.map fst
