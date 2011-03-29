open Ast
open Format
open Scanf

(* State *) (* {{{ *)

exception Error of string

type value = int
type variable = string

module type StackT = sig
  type t
  val empty : t
  val add_variable : t -> variable -> t
  val write : t -> variable -> value -> t
  val read : t -> variable -> value
end

module type HeapT = sig
  type t
  val empty : t
  val add_object : t -> variable list -> (t * value)
  val write : t -> value -> variable -> value -> t
  val read : t -> value -> variable -> value
end

let read_input () = scanf " %d" (fun x -> x)

(* implementation *) (* {{{ *)
module StringMap = Map.Make (String)
module IntMap = Map.Make (struct type t = int let compare = compare end)

module Stack : StackT = struct
  type t = value StringMap.t
  let empty = StringMap.empty
  let add_variable s v = StringMap.add v (read_input ()) s
  let write s v x = StringMap.add v x s
  let read s v = StringMap.find v s (* If Not_found, then fix typechecker. *)
end

module Heap : HeapT = struct
  type t = int StringMap.t IntMap.t * int

  let empty = IntMap.empty, 0

  let add_object (h, cnt) fs =
    let add_field s f = StringMap.add f (read_input ()) s in
    let s = List.fold_left add_field StringMap.empty fs in
    let h = IntMap.add cnt s h in
    ((h, succ cnt), cnt)

  let write (h, cnt) p f x =
    try
      let fs = IntMap.find p h in
      assert (StringMap.mem f fs); (* otherwise, fix typechecker *)
      let fs = StringMap.add f x fs in
      (IntMap.add p fs h, cnt)
    with Not_found -> raise (Error "bad write")

  let read (h, cnt) p f =
    try
      let fs = IntMap.find p h in
      assert (StringMap.mem f fs); (* otherwise, fix typechecker *)
      StringMap.find f fs
    with Not_found -> raise (Error "bad read")
end
(* }}} *)

type state = 
  { globals : Stack.t
  ; heap : Heap.t
  ; locals : Stack.t }

(* }}} *)
(* interpreter *) (* {{{ *)
(* global environment *) (* {{{ *)
module StringSet = Set.Make (String)

module StringPairMap = Map.Make (struct
  type t = string * string
  let compare = compare
end)

let fields = ref StringMap.empty 
  (* for each class, a list of fields *)
let methods = ref StringPairMap.empty 
  (* for each (class, method) names, the method *)

let preprocess cs =
  fields := StringMap.empty;  methods := StringPairMap.empty;
  let preprocess_class (c, ms) =
    let fs = ref StringSet.empty in
    let preprocess_member = function
      | Field { declaration_variable = f; declaration_type = _ } ->
          assert (not (StringSet.mem f !fs)); (* otherwise fix tc.ml *)
          fs := StringSet.add f !fs
      | Method m -> 
          let k = c, m.method_name in
          assert (not (StringPairMap.mem k !methods)); (* otherwise fix tc.ml *)
          methods := StringPairMap.add k m !methods in
    List.iter preprocess_member ms;
    fields := StringMap.add c (StringSet.elements !fs) !fields in
  List.iter preprocess_class cs

(* }}} *)
(* helpers *) (* {{{ *)

let vars ds = List.map (fun x -> x.declaration_variable) ds

let assign_value state x v =
  begin try 
      { state with locals = Stack.write state.locals x v }, None
    with Error _ -> begin try
      let this = Stack.read state.locals "this" in
      { state with heap = Heap.write state.heap this x v }, None
    with Error _ ->
      { state with globals = Stack.write state.globals x v }, None
  end end

(* }}} *)

let rec expression state = function
  (* XXX must take last bit before doing boolean operations *)
  | Ac (Or, xs) -> List.fold_left max 0 (List.map (expression state) xs)
  | Ac (And, xs) -> List.fold_left min 1 (List.map (expression state) xs)
  | Bin (l, op, r) ->
      let l = expression state l in
      let r = expression state r in
      if (l = r) = (op = Eq) then 1 else 0
  | Not e -> 1 - expression state e
  | Deref (_, _) -> eprintf "@[todo: expression Deref@."; 0
  | Ref _ -> eprintf "@[todo: expression Ref@."; 0
  | Literal None -> read_input ()
  | Literal (Some x) -> x

let rec assignment state x e =
  assign_value state x (expression state e)

and call state c = 
  let k = Util.from_some c.call_class, c.call_method in
  let m = StringPairMap.find k !methods in
  let formals = "this" :: vars m.method_formals in
  let actuals = c.call_receiver :: c.call_arguments in
  let actuals = List.map (expression state) actuals in 
  let new_locals = List.fold_left2 Stack.write Stack.empty formals actuals in
  let old_locals = state.locals in
  let state, value = body { state with locals = new_locals } m.method_body in
  let state = { state with locals = old_locals } in
  match c.call_lhs with
    | Some x -> assign_value state x (Util.from_some value)
    | None -> state, None

and allocate state { allocate_lhs = x; allocate_type = t} =
  match Util.from_some t with
    | Unit -> assign_value state x 0
    | Bool -> assign_value state x (read_input () land 1)
    | Class c ->
        let fields = StringMap.find c !fields in
        let nh, no = Heap.add_object state.heap fields in
        let ns = { state with heap = nh } in
        assign_value ns x no
    | AnyType -> 
        failwith "Huh? Only literals are polymorphic, and they're not on lhs."

and while_ state 
  { while_pre_body = _
  ; while_condition = _
  ; while_post_body = _ }
=
  eprintf "@[todo: while@."; state, None

and if_ state _ _ =
  eprintf "@[todo: if@."; state, None

and statement state = function
  | Return e -> (state, Some (expression state e))
  | Assignment (x, e) -> assignment state x e
  | Call c -> call state c
  | Allocate a -> allocate state a
  | While w -> while_ state w
  | If (c, b) -> if_ state c b

and body state (Body (ds, ss)) =
  let state = { state with 
    locals = List.fold_left Stack.add_variable state.locals (vars ds) } in
  let f acc { ast = s; line = _ } = match acc with
    | (state, None) -> statement state s
    | x -> x in
  List.fold_left f (state, None) ss

let program p =
  let gs = vars p.program_globals in
  let globals = List.fold_left Stack.add_variable Stack.empty gs in
  let state = { globals = globals; heap = Heap.empty; locals = Stack.empty } in
  preprocess p.program_classes;
  body state p.program_main

(* }}} *)
(* driver *) (* {{{ *)

let interpret fn =
  let f = open_in fn in
  let lexbuf = Lexing.from_channel f in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  try
    let p = parse (Lexer.token lexbuf) in
    ignore (Tc.program p);
    ignore (program p)
  with
    | Parser.Error ->
        (match Lexing.lexeme_start_p lexbuf with 
        { Lexing.pos_lnum=line; Lexing.pos_bol=c0;
          Lexing.pos_fname=_; Lexing.pos_cnum=c1} ->
        eprintf "@[%d:%d: parse error@." line (c1-c0+1))
    | Tc.Error e -> eprintf "@[%s (typecheck)@." e

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    interpret Sys.argv.(i)
  done
(* }}} *)
