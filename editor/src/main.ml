(* main.ml - Main entry point for the editor with enhanced terminal handling *)

open Types

(** Display module for rendering the editor *)
module Display = struct
  (** Draw status line *)
  let draw_status_line state _config =
    let filename = match state.filename with
      | Some f -> f
      | None -> "[No Name]"
    in
    let modified = if state.modified then " [+]" else "" in

    (* Add mode indicator *)
    let mode_str = match Editor.get_mode state with
      | GLProofMode -> " [GL]"
      | ReflectionMode -> " [REFL]"
      | FunctionalMode -> " [FUNC]"
    in

    let position = Printf.sprintf " %d:%d " 
      (state.cursor.row + 1) (state.cursor.col + 1) in
    
    (* Add debug info about syntax *)
    let syntax_info = match state.syntax with
      | Some s -> Printf.sprintf " [%d tokens, %d errors]" 
          (List.length s.tokens) (List.length s.errors)
      | None -> " [No syntax]"
    in
    
    let left = Printf.sprintf " %s%s%s%s" filename modified mode_str syntax_info in
    let padding_len = max 0 (state.window_width - 
                            String.length left - String.length position) in
    let padding = String.make padding_len ' ' in
    
    Terminal.move_cursor (state.window_height - 2) 0;
    Terminal.set_attributes [Reverse];
    print_string left;
    print_string padding;
    print_string position;
    Terminal.set_attributes []
  
  (** Draw message line *)
  let draw_message_line state =
    Terminal.move_cursor (state.window_height - 1) 0;
    Terminal.clear_line ();
    Terminal.set_attributes [FgColor Yellow];
    print_string state.status_message;
    Terminal.set_attributes []
  
  (** Draw line numbers *)
  let draw_line_number row current_row config =
    if config.show_line_numbers then begin
      let attrs = 
        if row = current_row then 
          [FgColor BrightYellow; Bold]
        else 
          [FgColor BrightBlack] 
      in
    
      Terminal.set_attributes attrs;
      Printf.printf "%4d " (row + 1);
      Terminal.set_attributes []
    end
  
  (** Draw text line with enhanced syntax highlighting *)
  let draw_line state config screen_row file_row =
    Terminal.move_cursor screen_row 0;
    Terminal.clear_line ();
    
    (* Draw line number *)
    draw_line_number file_row state.cursor.row config;
    
    (* Draw text content with highlighting *)
    if file_row < Array.length state.lines then begin
      let line = state.lines.(file_row) in
      let visible_start = state.view_offset.col in
      let visible_len = min (String.length line - visible_start) 
                           (state.window_width - 6) in
      
      if visible_len > 0 && visible_start < String.length line then begin
        (* Ensure syntax analysis exists *)
        let syntax = match state.syntax with
          | Some s -> s
          | None -> Syntax.analyze state.lines
        in
        
        (* Check if cursor is on a brace to find its match *)
        let matching_pos = 
          if file_row = state.cursor.row then
            Syntax.matching_brace syntax state.cursor
          else None
        in
        
        (* Get identifiers to highlight if cursor is on one *)
        let highlight_idents = 
          if file_row = state.cursor.row then
            Syntax.same_identifier_tokens syntax state.cursor
          else []
        in
        
        (* Process each visible character *)
        for i = visible_start to min (String.length line - 1) (visible_start + visible_len - 1) do
          let c = String.get line i in
          let pos = { Types.row = file_row; Types.col = i } in
          
          (* Find token at this position *)
          let token_opt = List.find_opt (fun tok ->
            tok.Types.start_pos.row = file_row &&
            tok.Types.start_pos.col <= i &&
            tok.Types.end_pos.col > i
          ) syntax.tokens in
          
          (* Get base attributes for this character *)
          let attrs = match token_opt with
            | Some tok ->
                (match List.find_opt (fun (t, _) -> t = tok.ttype) config.color_scheme with
                 | Some (_, attrs) -> attrs
                 | None -> [])
            | None -> []
          in
          
          (* Add current line subtle highlighting *)
          let attrs = 
            if file_row = state.cursor.row && config.highlight_current_line then
              attrs  (* Remove the background on current line for now *)
            else attrs
          in
          
          (* ENHANCED: Check if this is cursor position (for brace) *)
          let attrs =
            if file_row = state.cursor.row && i = state.cursor.col then
              match token_opt with
              | Some tok when (tok.ttype = Types.TLParen || tok.ttype = Types.TRParen ||
                              tok.ttype = Types.TLBrace || tok.ttype = Types.TRBrace ||
                              tok.ttype = Types.TLBracket || tok.ttype = Types.TRBracket) ->
                  (* Cursor is on a brace - highlight it *)
                  Types.BgColor Types.Magenta :: Types.Bold :: attrs
              | _ -> attrs
            else attrs
          in
          
          (* ENHANCED: Check if this position is the matching brace *)
          let attrs =
            match matching_pos with
            | Some mpos when mpos.row = file_row && mpos.col = i ->
                (* This is the matching brace *)
                Types.BgColor Types.Magenta :: Types.Bold :: attrs
            | _ -> attrs
          in
          
          (* ENHANCED: Check if this token is a highlighted identifier *)
          let attrs =
            match token_opt with
            | Some tok when tok.ttype = Types.TIdentifier ->
                if List.exists (fun t -> 
                  t.Types.start_pos.row = file_row && 
                  t.Types.start_pos.col <= i && 
                  t.Types.end_pos.col > i
                ) highlight_idents then
                  (* This identifier should be highlighted *)
                  Types.BgColor Types.BrightBlack :: Types.Underline :: attrs
                else attrs
            | _ -> attrs
          in
          
          (* Check for syntax errors *)
          let attrs =
            if Syntax.has_error_at syntax pos then
              Types.FgColor Types.Red :: Types.Underline :: Types.Bold :: attrs
            else attrs
          in
          
          (* Apply attributes and print character *)
          Terminal.set_attributes attrs;
          print_char c
        done;
        
        Terminal.set_attributes []
      end
    end else begin
      (* Empty line indicator *)
      Terminal.set_attributes [FgColor BrightBlack];
      print_string "~";
      Terminal.set_attributes []
    end
  
  (** Render the entire editor *)
  let render state config =
    Terminal.hide_cursor ();
    
    (* Draw all visible lines *)
    let view_height = state.window_height - 2 in
    for screen_row = 0 to view_height - 1 do
      let file_row = state.view_offset.row + screen_row in
      draw_line state config screen_row file_row
    done;
    
    (* Draw status and message lines *)
    draw_status_line state config;
    draw_message_line state;
    
    (* Position cursor *)
    let cursor_screen_row = state.cursor.row - state.view_offset.row in
    let cursor_screen_col = 
      (if config.show_line_numbers then 5 else 0) + 
      state.cursor.col - state.view_offset.col 
    in
    Terminal.move_cursor cursor_screen_row cursor_screen_col;
    Terminal.show_cursor ();
    flush Stdlib.stdout
end

(** Main event loop with enhanced terminal handling *)
let rec event_loop state config =
  (* Render the current state *)
  Display.render state config;
  
  (* Read and process input *)
  match Terminal.read_key () with
  | None -> event_loop state config
  | Some key ->
      let new_state = match key with
        (* Cursor movement *)
        | `Up -> Editor.move_cursor state Up
        | `Down -> Editor.move_cursor state Down
        | `Left -> Editor.move_cursor state Left
        | `Right -> Editor.move_cursor state Right
        | `Home -> Editor.move_cursor state Home
        | `End -> Editor.move_cursor state End
        | `PageUp -> Editor.move_cursor state PageUp
        | `PageDown -> Editor.move_cursor state PageDown
        
        (* Text editing *)
        | `Char c -> Editor.insert_char state c
        | `Enter -> Editor.new_line state
        | `Backspace -> Editor.backspace state
        | `Tab -> Editor.insert_tab state config
        
        (* Control keys *)
        | `Control 'Q' | `Control 'q' -> 
            if state.modified then
              { state with 
                status_message = "Unsaved changes! Press Ctrl-Q again to quit, or Ctrl-X to force quit" }
            else
              raise Exit
        | `Control 'S' | `Control 's' -> Editor.save_file state
        | `Control 'D' | `Control 'd' -> Editor.delete_char state
        | `Control 'X' | `Control 'x' -> 
            (* Alternative quit command *)
            if state.modified then
              { state with 
                status_message = "Unsaved changes! Press Ctrl-X again to quit" }
            else
              raise Exit

        | `Control 'R' | `Control 'r' ->      
            (* Run the program through Task 2 interpreter *)
            Interpreter_integration.run_with_output_window state

        (* Enhanced GL-specific commands *)
        | `Control 'L' | `Control 'l' ->
            (* L for "Lint" - Check GL syntax *)
            let mode = Editor.get_mode state in
            if mode = GLProofMode then (
              let result = Gl_syntax.check_gl_syntax state.lines in
              let message = match result with
                | Ok msg -> "GL Syntax: " ^ msg
                | Error err -> "GL Error: " ^ err
              in
              { state with status_message = message }
            ) else
              { state with status_message = "Not in GL mode (file must end with .gl)" }
            
        | `Control 'E' | `Control 'e' ->
            (* E for "Evaluate" - Run GL demo with proper screen management *)
            let mode = Editor.get_mode state in
            if mode = GLProofMode then (
              (* Clear screen and show demo *)
              Terminal.clear_and_home ();
              Terminal.show_cursor ();
              
              (* Display header *)
              Terminal.set_attributes [Bold; FgColor Green];
              print_string "═══════════════════════════════════════════════\r\n";
              Printf.printf " Running GL Proof Demo: %s\r\n" 
                (match state.filename with Some f -> f | None -> "unnamed");
              print_string "═══════════════════════════════════════════════\r\n";
              Terminal.set_attributes [];
              print_string "\r\n";
              flush Stdlib.stdout;
              
              (* Run the demo *)
              (try 
                 Gl_syntax.run_gl_demo ();
                 print_string "\r\n";
                 Terminal.set_attributes [Bold; FgColor Green];
                 print_string "═══════════════════════════════════════════════\r\n";
                 print_string " ✓ GL proof demonstration completed successfully!\r\n";
               with e -> 
                 print_string "\r\n";
                 Terminal.set_attributes [Bold; FgColor Red];
                 print_string "═══════════════════════════════════════════════\r\n";
                 Printf.printf " ✗ GL demo error: %s\r\n" (Printexc.to_string e));
              
              print_string " Press any key to return to editor...\r\n";
              print_string "═══════════════════════════════════════════════\r\n";
              Terminal.set_attributes [];
              flush Stdlib.stdout;
              
              (* Wait for keypress *)
              ignore (Terminal.read_key ());
              
              (* Return to editor cleanly *)
              Terminal.clear_and_home ();
              { state with status_message = "GL demo completed" }
            ) else
              { state with status_message = "Not in GL mode" }
            
        | `Control 'H' | `Control 'h' ->
            (* H for "Help" - Show commands based on file type *)
            let mode = Editor.get_mode state in
            let help_msg = match mode with
              | GLProofMode -> "GL Mode: Ctrl-L: Check syntax | Ctrl-E: Run demo | Ctrl-S: Save | Ctrl-X: Quit"
              | ReflectionMode -> "Reflection Mode: Ctrl-R: Run examples | Ctrl-S: Save | Ctrl-X: Quit"
              | FunctionalMode -> "Functional Mode: Ctrl-R: Run interpreter | Ctrl-S: Save | Ctrl-X: Quit"
            in
            { state with status_message = help_msg }
        
        (* Other keys *)
        | `Escape -> 
            if String.length state.status_message > 0 && 
               String.get state.status_message 0 = 'U' then  (* "Unsaved..." *)
              raise Exit  (* Allow Escape to force quit after warning *)
            else
              { state with status_message = "" }
        | _ -> state
      in
      
      (* Ensure syntax is updated after text changes *)
      let new_state = 
        if new_state.modified && new_state.syntax = None then
          { new_state with syntax = Some (Syntax.analyze new_state.lines) }
        else new_state
      in
      
      (* Check for force quit *)
      if (key = `Control 'Q' || key = `Control 'q' || 
          key = `Control 'X' || key = `Control 'x' || 
          key = `Escape) && 
         (String.length new_state.status_message > 0 &&
          String.get new_state.status_message 0 = 'U') then
        raise Exit
      else
        event_loop new_state config

(** Prompt for filename *)
let _prompt_filename message =
  Terminal.move_cursor (Terminal.get_size () |> fst |> pred) 0;
  Terminal.clear_line ();
  print_string message;
  flush Stdlib.stdout;
  Terminal.show_cursor ();
  
  let rec read_chars acc =
    match Terminal.read_key () with
    | Some (`Char c) -> 
        print_char c; 
        flush Stdlib.stdout;
        read_chars (acc ^ String.make 1 c)
    | Some `Enter -> acc
    | Some `Backspace when String.length acc > 0 ->
        print_string "\b \b";
        flush Stdlib.stdout;
        read_chars (String.sub acc 0 (String.length acc - 1))
    | Some `Escape -> ""
    | _ -> read_chars acc
  in
  
  Terminal.hide_cursor ();
  read_chars ""

(** Main function with enhanced initialization *)
let main () =
  (* Parse command line arguments *)
  let filename = 
    if Array.length Sys.argv > 1 then 
      Some Sys.argv.(1) 
    else 
      None 
  in
  
  (* Use enhanced terminal initialization *)
  Terminal.init_enhanced ();
  
  (* Set up cleanup on exit *)
  at_exit Terminal.cleanup;
  
  try
    (* Create initial state *)
    let initial_state = match filename with
      | Some f -> Editor.create_from_file f
      | None -> Editor.create_empty ()
    in
    
    (* Detect file mode and show appropriate help message *)
    let mode_message = match Editor.get_mode initial_state with
      | GLProofMode -> "GL Mode: Ctrl-L: Check | Ctrl-E: Run demo | Ctrl-H: Help | Ctrl-X: Quit"
      | ReflectionMode -> "Reflection Mode: Ctrl-R: Run | Ctrl-H: Help | Ctrl-X: Quit"
      | FunctionalMode -> "Functional Mode: Ctrl-R: Run | Ctrl-H: Help | Ctrl-X: Quit"
    in
    
    (* Ensure syntax analysis is initialized *)
    let initial_state = 
      let updated_state = { initial_state with status_message = mode_message } in
      if updated_state.syntax = None then
        { updated_state with syntax = Some (Syntax.analyze updated_state.lines) }
      else updated_state
    in
    
    (* Load configuration *)
    let config = Editor.default_config () in
    
    (* Start event loop *)
    event_loop initial_state config
  with
  | Exit -> 
      Terminal.cleanup ();
      print_endline "\nGoodbye!"
  | exn ->
      Terminal.cleanup ();
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1

(* Run the editor *)
let () = main ()