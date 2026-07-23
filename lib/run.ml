let print_result = function
  | Eval.VInt x -> print_int x
  | Eval.VBool b -> print_string (string_of_bool b)
  | Eval.VUnit -> print_string "()"
  | _ -> failwith "Unexpected eval result."

let exec raw =
  try
    raw |> Lex.lex |> Parse.parse
    |> List.iter (fun ast ->
        let result = Eval.eval ast Eval.Env.empty in
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
      | raw ->
          exec raw;
          repl ())

let run path =
  let src = open_in path |> In_channel.input_all in
  exec src
