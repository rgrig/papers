open Format

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program =
    MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  try program (Lexer.token lexbuf)
  with Parser.Error ->
    match Lexing.lexeme_start_p lexbuf with 
    { Lexing.pos_lnum=line; Lexing.pos_bol=c0;
      Lexing.pos_fname=_; Lexing.pos_cnum=c1} ->
    eprintf "@[%d:%d: parse error@." line (c1-c0+1)
