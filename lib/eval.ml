let is_terminal = function
  | Ast.Int _ | Ast.Bool _ | Ast.Var _ -> true
  | _ -> false

let step expr = expr

let rec eval expr =
  if is_terminal expr then expr else expr |> step |> eval
  
