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

let collect_methods es =
  let module Ssi = Set.Make (U.OrderedPair (String) (U.Int)) in
  let method_of_edge e =
    let l = e.PA.edge_label in
    (l.PA.label_method, PA.arg_count l.PA.label_data) in
  let f ms e = Ssi.add (method_of_edge e) ms in
  let uniq = List.fold_left f Ssi.empty es in
  Ssi.elements uniq

(* TODO(rgrig): Must add a Return edge from intermediate to source. *)
let split_call_return es =
  let methods = collect_methods es in
  let error_edges_from src =
    let one (mn, ac) =
      PA.mk_edge src "error" mn (PA.Call (U.replicate ac PA.GuardAny)) in
    List.map one methods in
  let desugar e =
    let l = e.PA.edge_label in
    match l.PA.label_data with
      | PA.Call_return (lr, la) ->
          let is = U.fresh_internal_id () in (* intermediate state *)
          let m = l.PA.label_method in
          PA.mk_edge e.PA.edge_source is m (PA.Call la) ::
          PA.mk_edge is e.PA.edge_target m (PA.Return (lr, List.length la)) ::
          error_edges_from is
      | _ -> [e] in
  List.concat (List.map desugar es)

let preprocess_properties ps =
  let ps = List.map (fun x -> x.ast) ps in
  automaton := ((* DBG eprintf "@[pick automaton@.";*) pick ok_automaton ps);
  automaton :=
    { !automaton with PA.edges = split_call_return !automaton.PA.edges }

let preprocess p =
  preprocess_classes p.program_classes;
  preprocess_properties p.program_properties

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

(* DBG
let dt = function
  | PA.Call _ -> eprintf "@[call@."
  | PA.Return _ -> eprintf "@[return@."
  | PA.Call_return _ -> eprintf "@[OMG@." *)

module PropertyInterpreter = struct
  exception No_match
  let pmatch s v =
    let chk c = if c then s else raise No_match in
    function
      | PA.Binder x -> Stack.init_variable s x v
      | PA.GuardVarEq x -> chk (v = Stack.read s x)
      | PA.GuardVarNeq x -> chk (v <> Stack.read s x)
      | PA.GuardCtEq w -> chk (v = w)
      | PA.GuardCtNeq w -> chk (v <> w)
      | PA.GuardAny -> chk true

  let evolve s
    { PA.label_method = en  (* the event taking place *)
    ; PA.label_data = ed }
    { PA.edge_source = src  (* the automaton edge being examined *)
    ; PA.edge_target = tgt
    ; PA.edge_label =
      { PA.label_method = ln
      ; PA.label_data = ld } }
  =
    if s.automaton_node <> src || en <> ln then None else
    try
(* DBG      dt ed; dt ld; *)
      let vs, ps = match ed, ld with
        | PA.Call vs, PA.Call ps -> List.map U.from_some vs, ps
        | PA.Return (None, m), PA.Return (PA.GuardAny, n) when m=n -> [],[]
        | PA.Return (Some v, m), PA.Return (p, n) when m = n -> [v], [p]
        | PA.Call_return _, _ | _, PA.Call_return _ ->
            failwith "INTERNAL: Call_return should be desugared by now."
        | _ -> raise No_match in
      Some { automaton_node = ((* DBG eprintf "@[    ->%s on %s@." tgt ln;*) tgt)
           ; automaton_stack = List.fold_left2 pmatch s.automaton_stack vs ps }
    with No_match | Invalid_argument _ -> None

  let check s e =
    let candidates = U.map_option (evolve s e) !automaton.PA.edges in
    let candidates = if candidates = [] then [s] else candidates in
    let next = ((* DBG eprintf "@[pick next state@.";*) pick automaton_start candidates) in
(* DBG    eprintf "@[  %s->%s@." s.automaton_node next.automaton_node; *)
(* DBG    report_error "transition"; *)
    if next.automaton_node = "error" then
      raise (Property_fails !automaton.PA.message);
    next

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
  let state = event state
    {PA.label_method=mn; PA.label_data=PA.Call (List.map (fun x->Some x) args)} in
  let state, value = body chk { state with locals = m_locals } m.method_body in
  let state = event state
    {PA.label_method=mn; PA.label_data=PA.Return(value,List.length args)} in
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
        eprintf "@[%d:%d: parse error@." line (c1-c0+1))
    | Check.Error -> ()

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    interpret Sys.argv.(i)
  done
(* }}} *)
