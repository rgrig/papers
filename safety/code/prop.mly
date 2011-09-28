(* Rules for the properties. *)

%{
  type pattern =
    | Action of PA.variable
    | GuardVar of PA.variable
    | GuardCt of PA.value
    | GuardAny

  let is_action s = 'A' <= s.[0] && s.[0] < 'Z'
  let var = String.uncapitalize

  let mk_guard ps = U.todo ()

  let mk_call_guard m ps = U.todo ()

  let mk_return_guard m p n = U.todo ()

  let mk_action ps =
    let f (a, i) = function
      | Action v -> (v, i) :: a, succ i
      | _ -> a, succ i in
    fst (List.fold_left f ([], 0) ps)
%}

%%

%public property: PROPERTY ID LC item* RC
      { { PA.name = "TODO"
        ; PA.message = "TODO"
        ; PA.transitions = [] } }

item:
    observing { () }
  | prefix { () }
  | message { () }
  | transition { () }

observing:
    OBSERVING GLOB string_pattern { () }
  | OBSERVING REGEXP string_pattern { () }

prefix: PREFIX string_pattern { () }

message: MESSAGE STRING { () }

transition:
    s=ID ARROW t=ID COLON ls=separated_nonempty_list(COMMA, label) { () }

label:
    RETURN value_pattern ASGN label_rhs(STAR)
  | RETURN label_rhs(STAR)
  | CALL STAR ASGN label_rhs(value_pattern)
  | CALL label_rhs(value_pattern)
  | label_rhs(value_pattern)
  | value_pattern ASGN label_rhs(value_pattern) { () }

label_rhs(ValuePattern):
    ValuePattern DOT method_pattern(ValuePattern) { () }
  | STAR { () }

method_pattern(ValuePattern):
    string_pattern args_pattern(ValuePattern) { () }
  | STAR { () }

args_pattern(Pattern):
    LB integer_pattern RB
  | LP separated_list(COMMA, Pattern) RP { () }

integer_pattern:
    NUMBER
  | STAR { () }

call_label:
    CALL r=value_pattern DOT m=ID LP a=separated_list(COMMA, value_pattern) RP
      { [ { PA.label_guard = mk_call_guard m (r :: a);
           PA.label_action = mk_action (r :: a) } ] }

return_label:
    RETURN r=value_pattern ASGN m=ID LB a=NUMBER RB
      { [ { PA.label_guard = mk_return_guard m r a;
            PA.label_action = mk_action [r] } ] }

mixed_label:
    l=value_pattern r=receiver? DOT m=ID LP a=separated_list(COMMA, value_pattern) RP
      { let l, r = match r with
          | None -> GuardAny, l
          | Some r -> l, r in [
        { PA.guard = mk_call_guard m (r :: a);
          PA.action = mk_action (r :: a) };
        { PA.guard = mk_return_guard m l (List.length a);
          PA.action = mk_action [l] } ] }

receiver: ASGN a=value_pattern { a }

string_pattern:
    ID { () }
  | CONSTANT { () }
  | STAR { () }

value_pattern:
    s=ID
      { if is_action s then Action (var s) else GuardVar s }
  | n=CONSTANT
      { GuardCt n }
  | STAR
      { GuardAny }

%%

