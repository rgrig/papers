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
  val del_object : t -> value -> t (* remove? *)
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

  let del_object (h, cnt) p =
    if not (IntMap.mem p h) then raise (Error "bad dealloc");
    (IntMap.remove p h, cnt)

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

let fields_by_class = ref StringMap.empty

(* }}} *)
(* interpreter *) (* {{{ *)
(* helpers *) (* {{{ *)

let process_tc_info aux =
  let process_class c fs =
    let collect f _ acc = f :: acc in
    let fs = StringMap.fold collect fs [] in
    fields_by_class := StringMap.add c fs !fields_by_class in
  StringMap.iter process_class aux

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

let expression _ _ = 
  eprintf "@[todo: expression@."; 0

let assignment state x e =
  assign_value state x (expression state e)

let call state _ =
  eprintf "@[todo: call@."; state, None

let allocate state x = function
  | Unit -> assign_value state x 0
  | Bool -> assign_value state x (read_input () land 1)
  | Class c ->
      let fields = StringMap.find c !fields_by_class in
      let nh, no = Heap.add_object state.heap fields in
      let ns = { state with heap = nh } in
      assign_value ns x no

let while_ state 
  { while_pre_body = _
  ; while_condition = _
  ; while_post_body = _ }
=
  eprintf "@[todo: while@."; state, None

let if_ state _ _ =
  eprintf "@[todo: if@."; state, None

let statement state = function
  | Return e -> (state, Some (expression state e))
  | Assignment (x, e) -> assignment state x e
  | Call c -> call state c
  | Allocate (v, t) -> allocate state v t
  | While w -> while_ state w
  | If (c, b) -> if_ state c b

let body state (Body (ds, ss)) =
  let ls = List.map (fun x -> x.declaration_variable) ds in
  let state = { state with 
    locals = List.fold_left Stack.add_variable state.locals ls } in
  let f acc { ast = s; line = _ } = match acc with
    | (state, None) -> statement state s
    | x -> x in
  List.fold_left f (state, None) ss

let program aux p =
  let gs = List.map (fun x -> x.declaration_variable) p.program_globals in
  let globals = List.fold_left Stack.add_variable Stack.empty gs in
  let state = { globals = globals; heap = Heap.empty; locals = Stack.empty } in
  process_tc_info aux;
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
    let aux = Tc.program p in
    ignore (program aux p)
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
