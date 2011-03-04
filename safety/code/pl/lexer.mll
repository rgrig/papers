{
  open Parser
}

rule token = parse
    [' ' '\t']+ { token lexbuf }
  | '\n'    { Lexing.new_line lexbuf; token lexbuf }
  | ":="    { ASGN }
  | ','     { COMMA }
  | '.'     { DOT }
  | "else"  { ELSE }
  | "if"    { IF }
  | '{'     { LB }
  | '('     { LP }
  | "new"   { NEW }
  | '}'     { RB }
  | "return"{ RETURN }
  | ')'     { RP }
  | "this"  { THIS }
  | "while" { WHILE }
  | ['a'-'z' 'A'-'Z']+ as id { ID(id) }
  | eof     { EOF }
  | _       { raise Error }
