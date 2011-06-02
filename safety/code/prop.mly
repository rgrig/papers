(* Rules for the properties. *)

%{
  module P = Ast.PropertyAst

  let is_binder s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize
%}

%token CALL

%%

%public property: PROPERTY m=STRING LB t=transition* RB {
  { P.message = m
  ; P.edges = List.concat t }
}

transition: s=ID ARROW t=ID COLON ls=separated_nonempty_list(COMMA, label) {
  let f l =
    { P.edge_source = s
    ; P.edge_target = t
    ; P.edge_label = l } in
  List.map f ls
}

label: l=call_label | l=return_label | l=mixed_label { l }

call_label:
    CALL r=pexpr DOT m=ID LP a=separated_list(COMMA, pexpr) RP
      { { P.label_method = m; P.label_data = P.Call (r :: a) } }

return_label:
    RETURN r=pexpr ASGN STAR DOT m=ID LP a=separated_list(COMMA, STAR) RP
      { { P.label_method = m; P.label_data = P.Return (r, List.length a) } }

mixed_label:
    l=pexpr r=receiver? DOT m=ID LP a=separated_list(COMMA, pexpr) RP
      { let l, r = match r with
          | None -> P.GuardAny, l
          | Some r -> l, r in
        { P.label_method = m; P.label_data = P.Call_return (l, r :: a) } }

receiver: ASGN a=pexpr { a }

pexpr:
    s=ID
      { if is_binder s then P.Binder (var s) else P.GuardVarEq s }
  | n=NUMBER
      { P.GuardCtEq n }
  | STAR
      { P.GuardAny }

%%

