(* interpreter_integration.ml *)

open Types

let interpreter_path = "~/reflection-types/interpreter/main.py"

(** Run the current file through the Task 2 interpreter *)
let run_program state =
  match state.filename with
  | None -> 
      { state with status_message = "Save file first (Ctrl-S) before running" }
  | Some filename ->
      (* Save the file first to ensure latest changes are executed *)
      let saved_state = 
        if state.modified then
          if File_ops.save_file filename state.lines then
            { state with modified = false }
          else
            { state with status_message = "Error saving file"; modified = state.modified }
        else
          state
      in
      
      if not saved_state.modified then
        (* Execute the interpreter *)
        let cmd = Printf.sprintf "python3 %s %s 2>&1" interpreter_path filename in
        let output = 
          try
            let ic = Unix.open_process_in cmd in
            let rec read_all acc =
              try
                let line = input_line ic in
                read_all (acc ^ line ^ "\n")
              with End_of_file -> acc
            in
            let result = read_all "" in
            let status = Unix.close_process_in ic in
            match status with
            | Unix.WEXITED 0 -> 
                (* Success *)
                Printf.sprintf "Output: %s" 
                  (if String.length result > 0 then result else "(no output)")
            | Unix.WEXITED code -> 
                (* Error *)
                Printf.sprintf "Error (exit code %d): %s" code result
            | _ -> 
                "Process terminated abnormally"
          with
          | Unix.Unix_error (err, _, _) ->
              Printf.sprintf "System error: %s" (Unix.error_message err)
          | exn ->
              Printf.sprintf "Error running interpreter: %s" (Printexc.to_string exn)
        in
        { saved_state with status_message = output }
      else
        saved_state

(** Execute current file and show output in a popup *)
let run_with_output_window state =
  match state.filename with
  | None -> 
      { state with status_message = "Save file first (Ctrl-S) before running" }
  | Some filename ->
      (* Save if modified *)
      let saved_state = 
        if state.modified then
          if File_ops.save_file filename state.lines then
            { state with modified = false }
          else state
        else state
      in
      
      if not saved_state.modified then
        (* Run the interpreter and capture output first *)
        let cmd = Printf.sprintf "python3 %s %s 2>&1" interpreter_path filename in
        let output_lines = ref [] in
        let exit_status = 
          try
            let ic = Unix.open_process_in cmd in
            let rec read_lines () =
              try
                let line = input_line ic in
                output_lines := line :: !output_lines;
                read_lines ()
              with End_of_file -> ()
            in
            read_lines ();
            Unix.close_process_in ic
          with _ -> Unix.WEXITED 1
        in
        let output_lines = List.rev !output_lines in
        
        (* Now display everything cleanly *)
        Terminal.clear_screen ();
        Terminal.set_attributes [];
        print_string "\r\n";  (* Ensure we start fresh *)
        
        (* Header *)
        Terminal.set_attributes [Types.Bold; Types.FgColor Types.Green];
        print_string "═══════════════════════════════════════════════\r\n";
        Printf.printf " Running: %s\r\n" filename;
        print_string "═══════════════════════════════════════════════\r\n";
        Terminal.set_attributes [];
        print_string "\r\n";
        
        (* Output *)
        List.iter (fun line -> Printf.printf "%s\r\n" line) output_lines;
        
        (* Footer *)
        print_string "\r\n";
        Terminal.set_attributes [Types.Bold; Types.FgColor Types.Green];
        print_string "═══════════════════════════════════════════════\r\n";
        (match exit_status with
         | Unix.WEXITED 0 -> print_string " ✓ Program completed successfully\r\n"
         | Unix.WEXITED n -> Printf.printf " ✗ Program exited with code: %d\r\n" n
         | _ -> print_string " ✗ Program terminated abnormally\r\n");
        print_string " Press any key to return to editor...\r\n";
        print_string "═══════════════════════════════════════════════\r\n";
        Terminal.set_attributes [];
        flush Stdlib.stdout;
        
        (* Wait for keypress *)
        ignore (Terminal.read_key ());
        
        (* Clear and return to editor *)
        Terminal.clear_screen ();
        saved_state
      else
        saved_state

(** Check if interpreter is available *)
let check_interpreter () =
  Sys.file_exists interpreter_path ||
  Sys.file_exists "~/reflection-types/interpreter/main.py"