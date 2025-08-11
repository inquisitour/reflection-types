(* editor_gadts.ml - Editor examples for refelction types *)

(* Simple position type for standalone testing *)
module TestTypes = struct
  type position = { row: int; col: int }
  type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
               | BrightBlack | BrightRed | BrightGreen | BrightYellow 
               | BrightBlue | BrightMagenta | BrightCyan | BrightWhite
               | Default
  type text_attribute = 
    | Bold | Underline | Italic | Reverse 
    | FgColor of color | BgColor of color
end

open TestTypes

(* ============================================================ *)
(* Type-Safe Brace Matching                                    *)
(* ============================================================ *)

module BraceMatching = struct
  (* Brace types that must match *)
  type _ brace =
    | Paren : [`Open | `Close] -> unit brace
    | Brace : [`Open | `Close] -> [`Lazy] brace
    | Bracket : [`Open | `Close] -> [`Eager] brace
    
  (* Proof that two braces match *)
  type (_, _) matches = Match : ('a, 'a) matches
  
  (* Check if braces match at compile time *)
  let check_match : type a b. a brace -> b brace -> (a, b) matches option =
    fun b1 b2 ->
      match b1, b2 with
      | Paren `Open, Paren `Close -> Some Match
      | Paren `Close, Paren `Open -> Some Match
      | Brace `Open, Brace `Close -> Some Match
      | Brace `Close, Brace `Open -> Some Match
      | Bracket `Open, Bracket `Close -> Some Match
      | Bracket `Close, Bracket `Open -> Some Match
      | _ -> None
      
  (* Find matching brace in a list - fixed the type variable issue *)
  type located_brace = 
    | LParen : [`Open | `Close] * position -> located_brace
    | LBrace : [`Open | `Close] * position -> located_brace
    | LBracket : [`Open | `Close] * position -> located_brace
  
  let find_matching_paren (dir : [`Open | `Close])
                          (braces : located_brace list) : position option =
    let rec search = function
      | [] -> None
      | LParen (d, pos) :: rest ->
          if (dir = `Open && d = `Close) || (dir = `Close && d = `Open) then
            Some pos
          else search rest
      | _ :: rest -> search rest
    in search braces
end

(* ============================================================ *)
(* Type-Safe Command History (Undo/Redo)                       *)
(* ============================================================ *)

module History = struct
  (* Commands with their inverse operations *)
  type _ reversible_command =
    | RInsert : string * position -> string reversible_command
    | RDelete : string * position -> string reversible_command
    | RReplace : string * string * position -> (string * string) reversible_command
    
  (* Get the inverse of a command *)
  let inverse : type a. a reversible_command -> a reversible_command = function
    | RInsert (text, pos) -> RDelete (text, pos)
    | RDelete (text, pos) -> RInsert (text, pos)
    | RReplace (old_text, new_text, pos) -> RReplace (new_text, old_text, pos)
    
  (* Simplified history type *)
  type history = {
    undo_stack: (unit -> unit) list;
    redo_stack: (unit -> unit) list;
  }
  
  let empty_history = { undo_stack = []; redo_stack = [] }
  
  let record_action (_action : unit -> unit) (undo_action : unit -> unit) hist =
    { undo_stack = undo_action :: hist.undo_stack; redo_stack = [] }
    
  let undo hist =
    match hist.undo_stack with
    | [] -> (hist, false)
    | action :: rest -> 
        action ();
        ({ undo_stack = rest; redo_stack = action :: hist.redo_stack }, true)
        
  let redo hist =
    match hist.redo_stack with
    | [] -> (hist, false)
    | action :: rest ->
        action ();
        ({ undo_stack = action :: hist.undo_stack; redo_stack = rest }, true)
end

(* ============================================================ *)
(* Type-Safe Token System                                      *)
(* ============================================================ *)

type blue
type white
type green
type yellow
type red
type brightblack

module TypedTokens = struct
  (* Tokens that encode their color requirements *)
  type _ token_color =
    | KeywordToken : blue token_color
    | IdentToken : white token_color
    | NumberToken : green token_color
    | StringToken : yellow token_color
    | OperatorToken : red token_color
    | CommentToken : brightblack token_color
  
  (* Convert token type to guaranteed color *)
  let token_to_color : type a. a token_color -> color = function
    | KeywordToken -> Blue
    | IdentToken -> White  
    | NumberToken -> Green
    | StringToken -> Yellow
    | OperatorToken -> Red
    | CommentToken -> BrightBlack
    
  (* Token with position and guaranteed color *)
  type colored_token = 
    | ColoredToken : 'a token_color * string * position -> colored_token
    
  (* Get attributes for a colored token *)
  let get_token_attrs (ColoredToken (tok_type, _, _)) =
    [FgColor (token_to_color tok_type)]
end

(* ============================================================ *)
(* Type-Safe Syntax Tree Visitor                              *)
(* ============================================================ *)

module SyntaxVisitor = struct
  (* AST nodes *)
  type _ node =
    | NInt : int -> int node
    | NString : string -> string node
    | NBool : bool -> bool node
    | NList : any_node list -> any_node node

  (* Existential wrapper for heterogeneous node lists *)
  and any_node = AnyNode : 'a node -> any_node

  (* Visitor that preserves type information *)
  type 'a visitor = {
    visit_int: int -> 'a;
    visit_string: string -> 'a;
    visit_bool: bool -> 'a;
    visit_list: 'a list -> 'a;
  }
    
  (* Type-safe visitor application *)
  let rec visit : type a. a node -> 'b visitor -> 'b = 
    fun node v ->
      match node with
      | NInt n -> v.visit_int n
      | NString s -> v.visit_string s
      | NBool b -> v.visit_bool b
      | NList nodes -> v.visit_list (List.map (fun (AnyNode n) -> visit n v) nodes)
      
  (* Example: convert to string *)
  let to_string_visitor = {
    visit_int = string_of_int;
    visit_string = (fun s -> "\"" ^ s ^ "\"");
    visit_bool = string_of_bool;
    visit_list = (fun xs -> "[" ^ String.concat "; " xs ^ "]");
  }
end

(* ============================================================ *)
(* Demo of Fixed Integration                                   *)
(* ============================================================ *)

let demo_integration () =
  print_endline "\n=== Fixed GADT Integration Examples ===\n";
  
  (* 1. Brace matching *)
  print_endline "1. Type-safe brace matching:";
  let braces = [
    BraceMatching.LParen (`Open, {row=0; col=5});
    BraceMatching.LParen (`Close, {row=0; col=10});
    BraceMatching.LBrace (`Open, {row=1; col=0});
    BraceMatching.LBrace (`Close, {row=5; col=0});
  ] in
  (match BraceMatching.find_matching_paren `Open braces with
   | Some pos -> Printf.printf "   Found match at (%d,%d)\n" pos.row pos.col
   | None -> print_endline "   No match found");
  
  (* 2. Type-safe tokens *)
  print_endline "\n2. Type-safe token coloring:";
  let tokens = [
    TypedTokens.ColoredToken (TypedTokens.KeywordToken, "let", {row=0; col=0});
    TypedTokens.ColoredToken (TypedTokens.IdentToken, "x", {row=0; col=4});
    TypedTokens.ColoredToken (TypedTokens.OperatorToken, "=", {row=0; col=6});
    TypedTokens.ColoredToken (TypedTokens.NumberToken, "42", {row=0; col=8});
  ] in
  List.iter (fun (TypedTokens.ColoredToken (_, text, pos)) ->
    Printf.printf "   '%s' at (%d,%d)\n" text pos.row pos.col
  ) tokens;
  
  (* 3. Command history *)
  print_endline "\n3. Type-safe command history:";
  let h0 = History.empty_history in
  let h1 = History.record_action 
    (fun () -> print_endline "   Executing: insert 'hello'")
    (fun () -> print_endline "   Undoing: delete 'hello'")
    h0 in
  let (_, success) = History.undo h1 in
  Printf.printf "   Undo successful: %b\n" success;
  
  (* 4. Syntax visitor *)
  print_endline "\n4. Type-safe syntax visitor:";
  let ast = SyntaxVisitor.NList [
    SyntaxVisitor.AnyNode (SyntaxVisitor.NInt 42);
    SyntaxVisitor.AnyNode (SyntaxVisitor.NString "hello");
    SyntaxVisitor.AnyNode (SyntaxVisitor.NBool true);
  ] in
  let result = SyntaxVisitor.visit ast SyntaxVisitor.to_string_visitor in
  Printf.printf "   AST as string: %s\n" result;
  
  print_endline "\n✅ All fixed examples work correctly!"

let () = demo_integration ()