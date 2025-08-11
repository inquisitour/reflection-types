(* highlight.ml - Syntax highlighting logic *)

open Types
open Gl_syntax


(** Get attributes for a token type from color scheme *)
let token_attributes config ttype =
  match List.find_opt (fun (t, _) -> t = ttype) config.color_scheme with
  | Some (_, attrs) -> attrs
  | None -> []

(** Create highlighted line from text and tokens *)
let highlight_line config syntax line_num line =
  let line_len = String.length line in
  
  (* Find all tokens on this line *)
  let line_tokens = List.filter (fun tok ->
    tok.start_pos.row = line_num
  ) syntax.tokens in
  
  (* Create array of highlighted characters *)
  Array.init line_len (fun col ->
    (* Find token at this position *)
    let token_opt = List.find_opt (fun tok ->
      tok.start_pos.row = line_num &&
      tok.start_pos.col <= col &&
      tok.end_pos.col > col
    ) line_tokens in
    
    (* Get base attributes from token type *)
    let base_attrs = match token_opt with
      | Some tok -> token_attributes config tok.ttype
      | None -> []
    in
    
    {
      chr = String.get line col;
      attrs = base_attrs;
    }
  )

(** Apply additional highlighting rules *)
let apply_highlight_rules config state syntax highlighted_lines =
  (* Helper to modify attributes at position *)
  let modify_at row col new_attrs =
    if row < Array.length highlighted_lines && 
       col < Array.length highlighted_lines.(row) then
      let char = highlighted_lines.(row).(col) in
      highlighted_lines.(row).(col) <- { char with 
        attrs = new_attrs @ char.attrs }
  in
  
  (* Highlight current line *)
  if config.highlight_current_line then begin
    if state.cursor.row < Array.length highlighted_lines then
      for col = 0 to Array.length highlighted_lines.(state.cursor.row) - 1 do
        modify_at state.cursor.row col [BgColor BrightBlack]
      done
  end;
  
  (* Highlight matching braces *)
  if config.highlight_matching_braces then begin
    match Syntax.matching_brace syntax state.cursor with
    | Some match_pos ->
        modify_at state.cursor.row state.cursor.col [BgColor Yellow; Bold];
        modify_at match_pos.row match_pos.col [BgColor Yellow; Bold]
    | None -> ()
  end;
  
  (* Highlight same identifiers *)
  if config.highlight_same_identifiers then begin
    let same_tokens = Syntax.same_identifier_tokens syntax state.cursor in
    List.iter (fun tok ->
      for col = tok.start_pos.col to tok.end_pos.col - 1 do
        modify_at tok.start_pos.row col [BgColor BrightBlack; Underline]
      done
    ) same_tokens
  end;
  
  (* Highlight errors *)
  List.iter (fun (_, err_pos) ->
    modify_at err_pos.row err_pos.col [FgColor Red; Underline; Bold]
  ) syntax.errors;
  
  highlighted_lines

(** Create fully highlighted text *)
let highlight_text config state =
  (* Analyze syntax if not cached *)
  let syntax = match state.syntax with
    | Some s -> s
    | None -> Syntax.analyze state.lines
  in
  
  (* Create base highlighting from tokens *)
  let highlighted = Array.mapi (fun line_num line ->
    highlight_line config syntax line_num line
  ) state.lines in
  
  (* Apply additional rules *)
  apply_highlight_rules config state syntax highlighted

(** Highlight GL code line *)
let highlight_gl_line line =
  (* Simple tokenization by spaces and special characters *)
  let rec tokenize acc current i =
    if i >= String.length line then
      if current <> "" then List.rev (current :: acc) else List.rev acc
    else
      let c = String.get line i in
      match c with
      | ' ' | '\t' | '\n' ->
          let acc' = if current <> "" then current :: acc else acc in
          tokenize acc' "" (i + 1)
      | '(' | ')' | '[' | ']' | '{' | '}' ->
          let acc' = if current <> "" then current :: acc else acc in
          tokenize (String.make 1 c :: acc') "" (i + 1)
      | _ ->
          tokenize acc (current ^ String.make 1 c) (i + 1)
  in
  let tokens = tokenize [] "" 0 in
  
  (* Map tokens to highlighted characters *)
  let rec process_tokens tokens col_offset result =
    match tokens with
    | [] -> List.rev result
    | token :: rest ->
        let style = 
          if List.mem token gl_keywords then [Bold; FgColor Blue]
          else if List.mem token gl_operators then [FgColor Cyan]
          else if Gl_syntax.starts_with "Axiom" token then [FgColor Green]
          else if Gl_syntax.starts_with "proof" token then [FgColor Green]
          else []
        in
        let chars = String.to_seq token |> List.of_seq in
        let highlighted_chars = List.map (fun c ->
          { chr = c; attrs = style }
        ) chars in
        (* Add space between tokens *)
        let space_char = { chr = ' '; attrs = [] } in
        process_tokens rest (col_offset + String.length token + 1) 
          (space_char :: (List.rev highlighted_chars) @ result)
  in
  
  if String.length line = 0 then
    [||]
  else
    let highlighted_list = process_tokens tokens 0 [] in
    Array.of_list (List.rev highlighted_list)
