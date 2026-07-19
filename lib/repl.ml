let print_result = function
  | Ast.Int x -> print_int x
  | Ast.Bool b -> print_string (string_of_bool b)
  | Ast.Unit -> print_string "()"
  | _ -> failwith "Unexpected eval result."

let handle_line raw =
  try
    raw |> Lex.lex |> Parse.parse
    |> List.iter (fun ast ->
        let result, _ = Eval.eval (ast, Eval.STable.empty) in
        print_result result;
        print_newline ())
  with Failure message | Invalid_argument message ->
    Printf.eprintf "Error: %s\n%!" message

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
      | raw -> handle_line raw;
        repl ())
