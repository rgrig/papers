open Format

let interpret p = () (* TODO *)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program =
    MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  try 
    let p = program (Lexer.token lexbuf) in
    ignore (Tc.program p);
    interpret p
  with 
    | Parser.Error ->
        (match Lexing.lexeme_start_p lexbuf with 
        { Lexing.pos_lnum=line; Lexing.pos_bol=c0;
          Lexing.pos_fname=_; Lexing.pos_cnum=c1} ->
        eprintf "@[%d:%d: parse error@." line (c1-c0+1))
    | Tc.Error e -> eprintf "@[%s (typecheck)@." e
