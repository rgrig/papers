(* Rules for the properties. *)

%{
  module P = Ast.PropertyAst

  type pattern =
    | Action of variable
    | GuardVar of variable
    | GuardCt of value
    | GuardAny

  let is_action s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize

  let mk_guard ps =
    let f (g, i) = function
      | GuardVar v -> P.Var (v, i) :: g, succ i
      | GuardCt x -> P.Ct (x, i) :: g, succ i
      | _ -> g, i in
    fst (List.fold_left f ([], 0) ps)

  let mk_call_guard m ps =
    let es = P.Event (P.Call, (m, List.length ps - 1)) :: mk_guard ps in
    let es = List.map (fun x -> P.Atomic x) es in
    P.And es

  let mk_return_guard m p n =
    let es = P.Event (P.Return, (m, n)) :: mk_guard [p] in
    let es = List.map (fun x -> P.Atomic x) es in
    P.And es

  let mk_action ps =
    let f (a, i) = function
      | Action v -> (v, i) :: a, succ i
      | _ -> a, succ i in
    fst (List.fold_left f ([], 0) ps)
%}

%token CALL

%%

%public property: PROPERTY m=STRING LC t=transition* RC {
  { P.message = m
  ; P.edges = List.concat t }
}

transition: s=ID ARROW t=ID COLON ls=separated_nonempty_list(COMMA, label) {
  let f l =
    { P.edge_source = s
    ; P.edge_target = t
    ; P.edge_labels = l } in
  List.map f ls
}

label: l=call_label | l=return_label | l=mixed_label { l }

call_label:
    CALL r=pexpr DOT m=ID LP a=separated_list(COMMA, pexpr) RP
      { [ { P.label_guard = mk_call_guard m (r :: a);
           P.label_action = mk_action (r :: a) } ] }

return_label:
    RETURN r=pexpr ASGN m=ID LB a=NUMBER RB
      { [ { P.label_guard = mk_return_guard m r a;
            P.label_action = mk_action [r] } ] }

mixed_label:
    l=pexpr r=receiver? DOT m=ID LP a=separated_list(COMMA, pexpr) RP
      { let l, r = match r with
          | None -> GuardAny, l
          | Some r -> l, r in [
        { P.label_guard = mk_call_guard m (r :: a);
          P.label_action = mk_action (r :: a) };
        { P.label_guard = mk_return_guard m l (List.length a);
          P.label_action = mk_action [l] } ] }

receiver: ASGN a=pexpr { a }

pexpr:
    s=ID
      { if is_action s then Action (var s) else GuardVar s }
  | n=NUMBER
      { GuardCt n }
  | STAR
      { GuardAny }

%%

