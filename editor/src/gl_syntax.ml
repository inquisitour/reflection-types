(* gl_syntax.ml - GL Syntax and Highlighting Support *)

open Types

(* GL keywords for syntax highlighting *)
let gl_keywords = [
  "Atom"; "True"; "False"; "Not"; "And"; "Or"; "Impl"; "Iff";
  "Box"; "BoxProp"; "AxiomK"; "AxiomS"; "AxiomDN"; "AxiomTrue";
  "AxiomLob"; "ModusPonens"; "Necessitation"; "Weaken";
  "proof"; "check"; "validate"; "frame"; "let"; "demo"
]

(* Operators and symbols *)
let gl_operators = [
  "->"; "<->"; "&&"; "||"; "~"; "="; "("; ")"; "["; "]"; "{"; "}"
]

(* Helper function for string prefix checking *)
let starts_with prefix str =
  let len_prefix = String.length prefix in
  let len_str = String.length str in
  len_prefix <= len_str &&
  String.sub str 0 len_prefix = prefix

(* Classify GL tokens *)
let classify_gl_token token =
  if List.mem token gl_keywords then GLKeyword
  else if List.mem token gl_operators then GLOperator
  else if starts_with "proof_" token then GLProof
  else if starts_with "Axiom" token then GLProof
  else GLFormula

(* Get highlighting for GL tokens *)
let gl_token_style = function
  | GLKeyword -> [Bold; FgColor Blue]
  | GLOperator -> [FgColor Cyan]
  | GLProof -> [FgColor Green]
  | GLFormula -> [FgColor Yellow]

(* Parse GL file to check for errors *)
let check_gl_syntax lines =
  (* Convert array to list *)
  let lines_list = Array.to_list lines in
  (* Simple bracket matching for now *)
  let rec count_parens count = function
    | [] -> count = 0
    | line::rest ->
        let open_count = String.fold_left (fun acc c -> 
          if c = '(' then acc + 1 else acc) 0 line in
        let close_count = String.fold_left (fun acc c -> 
          if c = ')' then acc + 1 else acc) 0 line in
        count_parens (count + open_count - close_count) rest
  in
  if count_parens 0 lines_list then
    Ok "Syntax OK"
  else
    Error "Unmatched parentheses"

(* Run GL proof checker demo - this is where we actually use gl_proof_checker *)
let run_gl_demo () =
  (* Call the demo function from gl_proof_checker *)
  Gl_proof_checker.demo ()