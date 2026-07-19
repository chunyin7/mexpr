let print_result = function
  | Ast.Int x -> print_int x
  | Ast.Bool b -> print_string (string_of_bool b)
  | Ast.Unit -> print_string "()"
  | _ -> failwith "Unexpected eval result."

let rec repl unit =
  print_string "> ";
  flush stdout;

  match read_line () with
  | exception End_of_file -> print_endline "\nExited."
  | line -> (
      let line = String.trim line in
      match line with
      | ":q" -> print_endline "\nExited."
      | "" -> repl ()
      | raw ->
          let asts = raw |> Lex.lex |> Parse.parse in

          let rec loop = function
            | hd :: tl -> (
                try
                  let result, _ = Eval.eval (hd, Eval.STable.empty) in
                  print_result result;
                  print_newline ()
                with Failure message ->
                  Printf.eprintf "Evaluation error: %s\n" message;
                  repl ())
            | _ -> ()
          in

          loop asts;
          repl ())
