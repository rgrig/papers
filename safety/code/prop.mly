(* Rules for the properties. *)

%{
  module P = Ast.Property

  let is_pattern s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize
%}

%%

%public property: PROPERTY m=STRING LB t=transition* RB {
  { Property.message = m
  ; Property.edges = List.concat t }
}

transition: s=ID ARROW t=ID COLON ls=separated_nonempty_list(COMMA, label) {
  let f l =
    { Property.edge_source = s
    ; Property.edge_target = t
    ; Property.edge_label = l } in
  List.map f ls
}

label: l=pexpr r=receiver? DOT m=ID LP a=separated_list(COMMA, pexpr) RP { 
  let l, r = 
    match r with
      | None -> Property.Pattern None, l
      | Some r -> l, r in 
  { P.label_result = l
  ; P.label_method = m
  ; P.label_arguments = r :: a }
}

receiver: ASGN a=pexpr { a }

pexpr:
    s=ID
      { if is_pattern s then Property.Pattern (Some (var s))
        else Property.Guard s }
  | n=NUMBER
      { Property.Constant n }
  | STAR
      { Property.Pattern None }
 
%%

