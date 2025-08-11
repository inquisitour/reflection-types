(* practical_gadts.ml - Practical examples for refelction types *)

(* ============================================================ *)
(* PART 1: Type-Safe AST for Your Task 2 Language              *)
(* ============================================================ *)

module TypedAST = struct
  (* Type-safe representation of your functional language *)
  type _ term =
    | Int : int -> int term
    | Bool : bool -> bool term
    | String : string -> string term
    | Lambda : string * 'a term -> ('b -> 'a) term
    | Apply : ('a -> 'b) term * 'a term -> 'b term
    | Plus : int term * int term -> int term
    | Minus : int term * int term -> int term
    | Mult : int term * int term -> int term
    | Div : int term * int term -> int term
    | Equal : 'a term * 'a term -> bool term
    | If : bool term * 'a term * 'a term -> 'a term
    | Let : string * 'a term * 'b term -> 'b term
    | Var : string -> 'a term
    
  (* Type-safe evaluator *)
  type value = 
    | VInt : int -> value
    | VBool : bool -> value
    | VString : string -> value
    | VClosure : (value -> value) -> value
    
  let rec eval : type a. (string * value) list -> a term -> value = 
    fun env -> function
    | Int n -> VInt n
    | Bool b -> VBool b
    | String s -> VString s
    | Plus (x, y) -> 
        (match eval env x, eval env y with
         | VInt a, VInt b -> VInt (a + b)
         | _ -> failwith "Type error in Plus")
    | Equal (x, y) -> VBool (eval env x = eval env y)
    | If (c, t, e) ->
        (match eval env c with
         | VBool true -> eval env t
         | VBool false -> eval env e
         | _ -> failwith "Type error in If")
    | _ -> failwith "Not fully implemented"
    
  (* Example: (if (5 = 5) then 10 else 20) + 30 *)
  let example = 
    Plus (
      If (Equal (Int 5, Int 5), Int 10, Int 20),
      Int 30
    )
end

(* ============================================================ *)
(* PART 2: Type-Safe Editor Commands with Undo/Redo            *)
(* ============================================================ *)

module EditorCommands = struct
  (* Phantom types for editor states *)
  type saved = Saved
  type modified = Modified
  type cursor_valid = CursorValid
  type cursor_invalid = CursorInvalid
  
  (* Editor state with phantom types *)
  type ('saved, 'cursor) editor_state = {
    content: string list;
    cursor_pos: int * int;
    filename: string option;
    saved_state: 'saved;
    cursor_state: 'cursor;
  }
  
  (* Commands that preserve type invariants *)
  type ('s1, 'c1, 's2, 'c2) command =
    | Insert : string -> (saved, cursor_valid, modified, cursor_valid) command
    | Delete : int -> (saved, cursor_valid, modified, cursor_invalid) command
    | Save : (modified, 'c, saved, 'c) command
    | MoveCursor : int * int -> ('s, cursor_invalid, 's, cursor_valid) command
    
  (* Type-safe command execution *)
  let execute : type s1 c1 s2 c2. 
    (s1, c1) editor_state -> 
    (s1, c1, s2, c2) command -> 
    (s2, c2) editor_state = 
    fun state cmd ->
      match cmd with
      | Insert text -> 
          { state with 
            content = text :: state.content; 
            saved_state = Modified;
            cursor_state = CursorValid }
      | Delete _ ->
          { state with
            content = (match state.content with [] -> [] | _::t -> t);
            saved_state = Modified;
            cursor_state = CursorInvalid }
      | Save -> 
          Printf.printf "Saving file...\n";
          { state with saved_state = Saved }
      | MoveCursor (x, y) ->
          { state with 
            cursor_pos = (x, y);
            cursor_state = CursorValid }
end

(* ============================================================ *)
(* PART 3: Type-Safe Syntax Highlighting                       *)
(* ============================================================ *)

module SyntaxHighlight = struct
  (* Token types with their expected value types *)
  type _ token =
    | TKeyword : string -> string token
    | TIdentifier : string -> string token  
    | TNumber : int -> int token
    | TString : string -> string token
    | TOperator : string -> string token
    | TComment : string -> string token
    | TLParen : unit token
    | TRParen : unit token
    | TLBrace : unit token
    | TRBrace : unit token

  (* Existential wrapper for heterogeneous token lists *)
  type any_token = AnyToken : _ token -> any_token
    
  (* Colors as types *)
  type color = Blue | Green | Red | Yellow | Gray | White
  
  (* Get color for token type *)
  let get_color : type a. a token -> color = function
    | TKeyword _ -> Blue
    | TIdentifier _ -> White
    | TNumber _ -> Green
    | TString _ -> Yellow
    | TOperator _ -> Red
    | TComment _ -> Gray
    | TLParen | TRParen | TLBrace | TRBrace -> White
    
  (* Highlight function that ensures correct coloring *)
  let highlight : type a. a token -> string = function
    | TKeyword s -> "\027[34m" ^ s ^ "\027[0m"  (* Blue *)
    | TIdentifier s -> "\027[37m" ^ s ^ "\027[0m"  (* White *)
    | TNumber n -> "\027[32m" ^ string_of_int n ^ "\027[0m"  (* Green *)
    | TString s -> "\027[33m\"" ^ s ^ "\"\027[0m"  (* Yellow *)
    | TOperator s -> "\027[31m" ^ s ^ "\027[0m"  (* Red *)
    | TComment s -> "\027[90m" ^ s ^ "\027[0m"  (* Gray *)
    | TLParen -> "\027[37m(\027[0m"
    | TRParen -> "\027[37m)\027[0m"
    | TLBrace -> "\027[37m{\027[0m"
    | TRBrace -> "\027[37m}\027[0m"
end

(* ============================================================ *)
(* PART 4: Type-Safe Configuration System - FIXED              *)
(* ============================================================ *)

module Config = struct
  (* Configuration keys with their types *)
  type _ config_key =
    | TabWidth : int config_key
    | ShowLineNumbers : bool config_key
    | Theme : string config_key
    | AutoSave : bool config_key
    | AutoSaveInterval : int config_key
    
  (* Heterogeneous list of configs *)
  type config_list = 
    | Nil : config_list
    | Cons : 'a config_key * 'a * config_list -> config_list
    
  (* Type-safe getter *)
  let rec get : type a. a config_key -> config_list -> a option =
    fun key configs ->
      match configs with
      | Nil -> None
      | Cons (k, v, rest) -> begin
          match key, k with
          | TabWidth, TabWidth -> Some v
          | ShowLineNumbers, ShowLineNumbers -> Some v
          | Theme, Theme -> Some v
          | AutoSave, AutoSave -> Some v
          | AutoSaveInterval, AutoSaveInterval -> Some v
          | _, _ -> get key rest
        end
      
  (* Type-safe setter *)
  let set : type a. a config_key -> a -> config_list -> config_list =
    fun key value configs ->
      Cons (key, value, configs)
      
  (* Default configuration *)
  let default_config = 
    Cons (TabWidth, 4,
      Cons (ShowLineNumbers, true,
        Cons (Theme, "dark",
          Cons (AutoSave, false,
            Cons (AutoSaveInterval, 60, Nil)))))
end

(* ============================================================ *)
(* DEMO: Run practical examples                                *)
(* ============================================================ *)

let demo () =
  print_endline "\n=== Practical GADT Examples for Editor/Interpreter ===\n";
  
  (* 1. Type-safe AST *)
  print_endline "1. Type-safe AST evaluation:";
  let result = TypedAST.eval [] TypedAST.example in
  (match result with
   | TypedAST.VInt n -> Printf.printf "   Result: %d\n" n
   | _ -> print_endline "   Unexpected result type");
  print_newline ();
  
  (* 2. Syntax highlighting *)
  print_endline "2. Type-safe syntax highlighting:";
  
  let tokens = [
    SyntaxHighlight.AnyToken (SyntaxHighlight.TKeyword "let");
    SyntaxHighlight.AnyToken (SyntaxHighlight.TIdentifier "x");
    SyntaxHighlight.AnyToken (SyntaxHighlight.TOperator "=");
    SyntaxHighlight.AnyToken (SyntaxHighlight.TNumber 42);
  ] in
  print_string "   ";
  List.iter (fun (SyntaxHighlight.AnyToken tok) -> 
    print_string (SyntaxHighlight.highlight tok);
    print_string " "
  ) tokens;
  print_endline "\n";
  
  (* 3. Configuration system *)
  print_endline "3. Type-safe configuration:";
  let config = Config.default_config in
  (match Config.get Config.TabWidth config with
   | Some n -> Printf.printf "   Tab width: %d\n" n
   | None -> ());
  (match Config.get Config.ShowLineNumbers config with
   | Some b -> Printf.printf "   Show line numbers: %b\n" b
   | None -> ());
  (match Config.get Config.Theme config with
   | Some s -> Printf.printf "   Theme: %s\n" s
   | None -> ());
  
  print_endline "\n✅ All practical examples completed!"

let () = demo ()