open Format

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try Parser.program Lexer.token lexbuf
  with Parser.Error ->
    match Lexing.lexeme_start_p lexbuf with 
    { Lexing.pos_lnum=line; Lexing.pos_bol=_;
      Lexing.pos_fname=_; Lexing.pos_cnum=_} ->
    eprintf "@[Parse error on line %d@." line
