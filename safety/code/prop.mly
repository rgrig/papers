(* Rules for the properties. *)

%{
  module P = Ast.PropertyAst

  let is_pattern s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize
%}

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

label: l=pexpr r=receiver? DOT m=ID LP a=separated_list(COMMA, pexpr) RP { 
  let l, r = 
    match r with
      | None -> P.Pattern None, l
      | Some r -> l, r in 
  { P.label_result = l
  ; P.label_method = m
  ; P.label_arguments = r :: a }
}

receiver: ASGN a=pexpr { a }

pexpr:
    s=ID
      { if is_pattern s then P.Pattern (Some (var s))
        else P.Guard s }
  | n=NUMBER
      { P.Constant n }
  | STAR
      { P.Pattern None }
 
%%

