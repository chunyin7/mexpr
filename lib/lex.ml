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
  | BANG
  | EOF

let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alpha c = is_digit c || is_letter c
let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

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

let word_to_tok = function
  | "let" -> LET
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "true" -> BOOL true
  | "false" -> BOOL false
  | value -> IDENT value

let lex src =
  let len = String.length src in

  let rec consume_while cur pred =
    if cur < len && pred src.[cur] then consume_while (cur + 1) pred else cur
  in

  let rec loop cur toks =
    if cur = len then List.rev (EOF :: toks)
    else
      match src.[cur] with
      | c when is_whitespace c -> loop (cur + 1) toks
      | c when is_digit c ->
          let next = consume_while (cur + 1) is_digit in
          let strval = String.sub src cur (next - cur) in
          loop next (INT (int_of_string strval) :: toks)
      | c when is_letter c ->
          let next = consume_while (cur + 1) is_alpha in
          let word = String.sub src cur (next - cur) in
          loop next (word_to_tok word :: toks)
      | '+' -> loop (cur + 1) (PLUS :: toks)
      | '-' -> loop (cur + 1) (MINUS :: toks)
      | '*' -> loop (cur + 1) (TIMES :: toks)
      | '/' -> loop (cur + 1) (DIVIDE :: toks)
      | '=' -> loop (cur + 1) (EQUAL :: toks)
      | '<' -> loop (cur + 1) (LESS :: toks)
      | '>' -> loop (cur + 1) (GREATER :: toks)
      | '(' -> loop (cur + 1) (LPAREN :: toks)
      | ')' -> loop (cur + 1) (RPAREN :: toks)
      | '!' -> loop (cur + 1) (BANG :: toks)
      | c ->
          invalid_arg (Printf.sprintf "invalid character %C at offset %d" c cur)
  in
  loop 0 []
