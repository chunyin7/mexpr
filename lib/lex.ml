type token =
  | INT of int
  | BOOL of bool
  | IDENT of string
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUAL
  | LESS
  | GREATER
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | LPAREN
  | RPAREN
  | EOF

let is_digit c = c >= '0' && c <= '9'

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let str_to_tok = function
  | "" -> EOF
  | "let" -> LET
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "true" -> BOOL true
  | "false" -> BOOL false
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> TIMES
  | "/" -> DIVIDE
  | "=" -> EQUAL
  | "<" -> LESS
  | ">" -> GREATER
  | "(" -> LPAREN
  | ")" -> RPAREN
  | value when String.for_all is_digit value -> INT (int_of_string value)
  | value
    when is_letter value.[0]
         && String.for_all (fun c -> is_letter c || is_digit c) value ->
      IDENT value
  | value -> invalid_arg ("invalid token: " ^ value)

let rec lex_loop src start cur toks =
  if src.[cur] = ' ' then lex_loop src (cur + 1) (cur + 1) (str_to_tok (String.sub src start (cur - start)) :: toks)
  else lex_loop src start (cur + 1) toks

let lex src =
  let len = String.length src in
  ()
