(* modules *) (* {{{ *)
open Ast
open Format
module PA = Ast.PropertyAst
module S = Stack (* save OCaml's stack module *)
module U = Util
(* }}} *)

(* State *) (* {{{ *)

exception Variable_missing
exception Bad_access
exception Property_fails of string

module type StackT = sig
  type t
  val empty : t

  (* These hide older variables with the same name. *)
  val init_variable : t -> variable -> value -> t
  val add_variable : t -> variable -> t
    (* [init_variable] with random value *)

  (* These two throw [Variable_missing] if the variable wasn't added earlier. *)
  val write : t -> variable -> value -> t
  val read : t -> variable -> value
end

module type HeapT = sig
  type t
  val empty : t
  val new_object : t -> variable list -> (t * value)

  (* These two throw [Bad_access] if the object is not allocated
     and [Variable_missing] if there's no field with that name. *)
  val write : t -> value -> variable -> value -> t
  val read : t -> value -> variable -> value
end

let read_input () = Scanf.scanf " %d" (fun x -> x)

(* implementation *) (* {{{ *)
module Stack : StackT = struct
  type t = value U.StringMap.t
  let empty = U.StringMap.empty
  let init_variable s x v = U.StringMap.add x v s
  let add_variable s x = init_variable s x (-1)
  let write s x v =
    if not (U.StringMap.mem x s) then raise Variable_missing;
    U.StringMap.add x v s
  let read s x =
    try U.StringMap.find x s
    with Not_found -> raise Variable_missing
end

module Heap : HeapT = struct
  type t = value U.StringMap.t U.IntMap.t * int

  let empty = U.IntMap.empty, 0

  let new_object (h, cnt) fs =
    let add_field s f = U.StringMap.add f (read_input ()) s in
    let s = List.fold_left add_field U.StringMap.empty fs in
    let h = U.IntMap.add cnt s h in
    ((h, succ cnt), cnt)

  let write (h, cnt) p f x =
    let fs = try U.IntMap.find p h with Not_found -> raise Bad_access in
    if not (U.StringMap.mem f fs) then raise Variable_missing;
    let fs = U.StringMap.add f x fs in
    (U.IntMap.add p fs h, cnt)

  let read (h, _) p f =
    let fs = try U.IntMap.find p h with Not_found -> raise Bad_access in
    try U.StringMap.find f fs with Not_found -> raise Variable_missing
end
(* }}} *)

type automaton_state =
  { automaton_node : string
  ; automaton_stack : Stack.t }

type 'a program_state =
  { globals : Stack.t
  ; heap : Heap.t
  ; locals : Stack.t
  ; automaton_state : 'a }
  (* Only [automaton_state program_state] is used, but forcing the polymorphic
  type ['a program_state] on some functions makes it possible to make sure
  they don't look at [automaton_state]. *)

let events = Queue.create ()

(* }}} *)
(* interpreter *) (* {{{ *)
(* helpers *) (* {{{ *)

let vars ds = List.map (fun x -> x.declaration_variable) ds

let assign_value state x v =
  begin try
      { state with locals = Stack.write state.locals x v }, None
    with Variable_missing -> begin try
      let this = Stack.read state.locals "this" in
      { state with heap = Heap.write state.heap this x v }, None
    with Variable_missing ->
      { state with globals = Stack.write state.globals x v }, None
  end end

let read_value state x =
  begin try
      Stack.read state.locals x
    with Variable_missing -> begin try
      Heap.read state.heap (Stack.read state.locals "this") x
    with Variable_missing ->
      Stack.read state.globals x
  end end

let pick x xs =
  let xs = x :: xs in
  let n = List.length xs in
  let m = read_input () mod n in
(* DBG eprintf "@[  %d out of %d@." (m+1) n; *)
  List.nth xs m

(* }}} *)
(* global environment *) (* {{{ *)
let automaton = ref ok_automaton
  (* the property being checked, picked randomly *)
let fields = ref U.StringMap.empty
  (* for each class, a list of fields *)
let methods = ref U.StringPairMap.empty
  (* for each (class, method) names, the method *)

let automaton_start =
  { automaton_node = "start"
  ; automaton_stack = Stack.empty }

let preprocess_classes cs =
  fields := U.StringMap.empty;  methods := U.StringPairMap.empty;
  let preprocess_class (c, ms) =
    let fs = ref U.StringSet.empty in
    let preprocess_member = function
      | Field { declaration_variable = f; declaration_type = _ } ->
          assert (not (U.StringSet.mem f !fs)); (* otherwise fix tc.ml *)
          fs := U.StringSet.add f !fs
      | Method m ->
          let k = c, m.method_name in
          assert (not (U.StringPairMap.mem k !methods)); (* otherwise fix tc.ml *)
          methods := U.StringPairMap.add k m !methods in
    List.iter preprocess_member ms;
    fields := U.StringMap.add c (U.StringSet.elements !fs) !fields in
  List.iter preprocess_class cs

let desugar_multi_edges es = es (* TODO
  failwith "TODO: make sure each edge has exactly one label." *)

let preprocess_properties ps =
  let ps = List.map (fun x -> x.ast) ps in
  automaton := ((* DBG eprintf "@[pick automaton@.";*) pick ok_automaton ps);
  automaton :=
    { !automaton with PA.edges = desugar_multi_edges !automaton.PA.edges }

let preprocess p =
  preprocess_classes p.program_classes;
  preprocess_properties p.program_properties

let is_tag = function
  | PA.Atomic (PA.Event _) -> true
  | PA.Not (PA.Atomic (PA.Event _)) -> true
  | _ -> false

(*
  Constructs a guard that holds when there exist event values and an automaton
  state that satisfy the given guard. All that remains tested is the event tag.
 *)
let mk_property_guard p =
  let guards = PA.guards_of_automaton p in
  let dnf = PA.dnf (PA.Or guards) in
  U.unique (List.map (List.filter is_tag) dnf)

(* }}} *)
(* error reporting *) (* {{{ *)

let program_name = ref ""
let location_stack = S.create ()
let report_error message =
  let location =
    try sprintf "@[%d@]" (S.top location_stack)
    with S.Empty -> "?" in
  eprintf "@[%s:%s: %s@." !program_name location message

(* }}} *)
(* functions that evolve only the automata state *) (* {{{ *)

module PropertyInterpreter = struct
  let evaluate_guard s e =
    let rv i = U.IntMap.find i e.PA.event_values in
    let rec f = function
      | PA.Atomic (PA.Var (x, i)) -> Stack.read s x = rv i
      | PA.Atomic (PA.Ct (v, i)) -> v = rv i
      | PA.Atomic (PA.Event et) -> et = e.PA.event_tag
      | PA.Atomic PA.Any -> true
      | PA.Not g -> not (f g)
      | PA.And gs -> List.for_all f gs
      | PA.Or gs -> List.exists f gs in
    f

  let perform_action s a e =
    let f s (v, i) =
      let vl = try U.IntMap.find i e.PA.event_values
      with Not_found -> failwith "Internal error: index not assigned" in
      Stack.init_variable s v vl in
    List.fold_left f s a

  let call_return_warn _ _ = failwith "todo"

  exception Return of Stack.t
  exception No_match
  let evolve
    { automaton_node = v
    ; automaton_stack = s }
    { PA.edge_source = src  (* the automaton edge being examined *)
    ; PA.edge_target = tgt
    ; PA.edge_labels = ls }
  =
    assert (src = v);
    let f (ls, s) e = match ls with
      | [] -> raise (Return s)
      | l :: ls ->
          let g = l.PA.label_guard in
          if not (evaluate_guard s e g) then begin
            if ls = [] then call_return_warn e g;
            raise No_match
          end;
          let s = perform_action s l.PA.label_action e in
          if ls = [] then raise (Return s);
          (ls, s) in
    try
      ignore (Queue.fold f (ls, s) events);
      failwith "internal error: previous line should always throw"
    with
      | No_match -> None
      | Return s ->
          Some ({ automaton_node = tgt; automaton_stack = s}, List.length ls)

  let check s e =
    Queue.push e events;
    let outgoing = PA.outgoing !automaton s.automaton_node in
    let n = List.fold_left max 0 (List.map PA.edge_length outgoing) in
    if Queue.length events >= n then begin
      let candidates = U.map_option (evolve s) outgoing in
      let candidates = if candidates = [] then [(s,1)] else candidates in
      let next, k = pick (automaton_start, 1) candidates in
      for i = 1 to k do ignore (Queue.pop events) done;
(* DBG   eprintf "@[  %s->%s@." s.automaton_node next.automaton_node; *)
(* DBG   report_error "transition"; *)
      if next.automaton_node = "error" then
        raise (Property_fails !automaton.PA.message);
      next
    end else s
end

(* }}} *)
(* functions that see only the program state *) (* {{{ *)

let rec expression (state : 'a program_state) =
  let bool_expression x =
    let r = expression state x in
    assert (0 <= r && r < 2);
    r in
  let convert_value x = function
    | Bool -> x land 1
    | Unit -> 0
    | _ -> x in
  function
    | Ac (Or, xs) -> List.fold_left max 0 (List.map bool_expression xs)
    | Ac (And, xs) -> List.fold_left min 1 (List.map bool_expression xs)
    | Bin (l, op, r) ->
        if (expression state l = expression state r) = (op = Eq) then 1 else 0
    | Not e -> (match expression state e with 0 -> 1 | _ -> 0)
    | Deref (e, f) -> Heap.read state.heap (expression state e) f
    | Ref x -> read_value state x
    | Literal (_, {contents=None}) -> failwith "INTERNAL: TC should fill this"
    | Literal (None, {contents=Some t}) -> convert_value (read_input ()) t
    | Literal (Some x, {contents=Some t}) -> convert_value x t

let rec assignment (state : 'a program_state) x e =
  assign_value state x (expression state e)

and call
  (chk : 'a -> 'b -> 'a)
  (state : 'a program_state)
  (c : call_statement)
=
  let event s e = { s with automaton_state = chk s.automaton_state e } in
  let m = (* method *)
    U.StringPairMap.find (U.from_some c.call_class, c.call_method) !methods in
  let mn = m.method_name in
  let formals = "this" :: vars m.method_formals in
  let args = List.map (expression state) (c.call_receiver::c.call_arguments) in
  let m_locals =
    List.fold_left2 Stack.init_variable Stack.empty formals args in
  let locals = state.locals in
  let method_id = (mn, List.length m.method_formals) in
  let state = event state (PA.mk_event PA.Call method_id args) in
  let state, value = body chk { state with locals = m_locals } m.method_body in
  let state = event state
    (PA.mk_event PA.Return method_id (U.list_of_option value)) in
  let state = { state with locals = locals } in
  match c.call_lhs with
    | Some x -> (* DBG eprintf "@[    %s returns %d@." c.call_method
    (U.from_some value);*) assign_value state x (U.from_some value)
    | None -> state, None

and allocate (state : 'a program_state) { allocate_lhs = x; allocate_type = t} =
  match U.from_some t with
    | Unit -> assign_value state x 0
    | Bool -> assign_value state x (read_input () land 1)
    | Class c ->
        let fields = U.StringMap.find c !fields in
        let nh, no = Heap.new_object state.heap fields in
        let ns = { state with heap = nh } in
        assign_value ns x no
    | AnyType _ ->
        failwith "Huh? Only literals are polymorphic, and they're not on lhs."

and while_ chk (state : 'a program_state) loop =
  let state, value = body chk state loop.while_pre_body in
  if value <> None then state, value else
  if expression state loop.while_condition land 1 = 0 then state, None else
  let state, value = body chk state loop.while_post_body in
  if value <> None then state, value else
  while_ chk state loop

and if_ chk (state : 'a program_state) c b =
  if expression state c land 1 <> 0 then
    body chk state b
  else
    state, None

and statement chk (state : 'a program_state) = function
  | Return e -> (state, Some (expression state e))
  | Assignment (x, e) -> assignment state x e
  | Call c -> call chk state c
  | Allocate a -> allocate state a
  | While w -> while_ chk state w
  | If (c, b) -> if_ chk state c b

and body chk (state : 'a program_state) (Body (ds, ss)) =
  let state = { state with
    locals = List.fold_left Stack.add_variable state.locals (vars ds) } in
  let f acc { ast = s; line = line } = match acc with
    | (state, None) ->
        S.push line location_stack;
        let r = statement chk state s in
        ignore (S.pop location_stack); r
    | x -> x in
  List.fold_left f (state, None) ss

(* }}} *)

let main state = U.option () (fun m ->
  try ignore (body PropertyInterpreter.check state m)
  with
    | Bad_access | Variable_missing -> report_error "memory fault"
    | Property_fails s -> report_error s)
  (* Exception Variable_missing at [x.f] when [x] points to an object with
     wrong type. This may happen with [var Foo x := *]. *)

let program p =
  let gs = vars p.program_globals in
  let globals = List.fold_left Stack.add_variable Stack.empty gs in
  let state =
    { globals = globals
    ; heap = Heap.empty
    ; locals = Stack.empty
    ; automaton_state = automaton_start } in
  preprocess p;
  main state p.program_main

(* }}} *)
(* driver *) (* {{{ *)

let interpret fn =
(* DBG eprintf "@[=== START ===@."; *)
  let f = open_in fn in
  let lexbuf = Lexing.from_channel f in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  program_name := fn;
  S.clear location_stack;
  try
    let p = parse (Lexer.token lexbuf) in
    Check.program !program_name p;
    ignore (program p)
  with
    | Parser.Error ->
        (match Lexing.lexeme_start_p lexbuf with
        { Lexing.pos_lnum=line; Lexing.pos_bol=c0;
          Lexing.pos_fname=_; Lexing.pos_cnum=c1} ->
        eprintf "@[%s:%d:%d: parse error@." fn line (c1-c0+1))
    | Check.Error -> ()

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    interpret Sys.argv.(i)
  done
(* }}} *)
