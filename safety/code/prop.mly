(* Rules for the properties. *)

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

label: l=atom r=receiver? DOT m=ID a=args { 
  let l, r = 
    match r with
      | None -> Literal None, l
      | Some r -> 
          (match l with Ref _ | Literal _ -> l, r | _ -> $syntaxerror) in
  { Property.label_lhs = l
  ; Property.label_receiver = r
  ; Property.label_method = m
  ; Property.label_arguments = a }
}

receiver: ASGN a=atom { a }

%%

