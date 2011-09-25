(* Rules for the properties. *)

%{
  type pattern =
    | Action of PA.variable
    | GuardVar of PA.variable
    | GuardCt of PA.value
    | GuardAny

  let is_action s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize

  let mk_guard ps =
    let f (g, i) = function
      | GuardVar v -> PA.Var (v, i) :: g, succ i
      | GuardCt x -> PA.Ct (x, i) :: g, succ i
      | _ -> g, i in
    fst (List.fold_left f ([], 0) ps)

  let mk_call_guard m ps =
    let es = PA.Event (PA.Call, (m, List.length ps - 1)) :: mk_guard ps in
    let es = List.map (fun x -> PA.Atomic x) es in
    PA.And es

  let mk_return_guard m p n =
    let es = PA.Event (PA.Return, (m, n)) :: mk_guard [p] in
    let es = List.map (fun x -> PA.Atomic x) es in
    PA.And es

  let mk_action ps =
    let f (a, i) = function
      | Action v -> (v, i) :: a, succ i
      | _ -> a, succ i in
    fst (List.fold_left f ([], 0) ps)
%}

%%

%public property: PROPERTY m=STRING LC observing* prefix* t=transition* RC {
  { PA.message = m
  ; PA.edges = List.concat t }
}

observing: OBSERVING { () }

prefix: PREFIX { () }

transition: s=ID ARROW t=ID COLON ls=separated_nonempty_list(COMMA, label) {
  let f l =
    { PA.edge_source = s
    ; PA.edge_target = t
    ; PA.edge_labels = l } in
  List.map f ls
}

label: l=call_label | l=return_label | l=mixed_label { l }

call_label:
    CALL r=pexpr DOT m=ID LP a=separated_list(COMMA, pexpr) RP
      { [ { PA.label_guard = mk_call_guard m (r :: a);
           PA.label_action = mk_action (r :: a) } ] }

return_label:
    RETURN r=pexpr ASGN m=ID LB a=NUMBER RB
      { [ { PA.label_guard = mk_return_guard m r a;
            PA.label_action = mk_action [r] } ] }

mixed_label:
    l=pexpr r=receiver? DOT m=ID LP a=separated_list(COMMA, pexpr) RP
      { let l, r = match r with
          | None -> GuardAny, l
          | Some r -> l, r in [
        { PA.label_guard = mk_call_guard m (r :: a);
          PA.label_action = mk_action (r :: a) };
        { PA.label_guard = mk_return_guard m l (List.length a);
          PA.label_action = mk_action [l] } ] }

receiver: ASGN a=pexpr { a }

pexpr:
    s=ID
      { if is_action s then Action (var s) else GuardVar s }
  | n=CONSTANT
      { GuardCt n }
  | STAR
      { GuardAny }

%%

