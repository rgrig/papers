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
  val del_object : t -> value -> t
  val write : t -> value -> variable -> value -> t
  val read : t -> value -> variable -> value
end

(* implementation *) (* {{{ *)
module StringMap = Map.Make (String)
module IntMap = Map.Make (struct type t = int let compare = compare end)

let read_input () = scanf " %d" (fun x -> x)

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
(* }}} *)

let body _ _ = failwith "continue here"

let program p =
  let gs = List.map (fun x -> x.declaration_variable) p.program_globals in
  let stack = List.fold_left Stack.add_variable Stack.empty gs in
  body (stack, Heap.empty) p.program_main

let _ =  (* main {{{ *)
  let lexbuf = Lexing.from_channel stdin in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  try 
    let p = parse (Lexer.token lexbuf) in
    ignore (Tc.program p);
    program p
  with 
    | Parser.Error ->
        (match Lexing.lexeme_start_p lexbuf with 
        { Lexing.pos_lnum=line; Lexing.pos_bol=c0;
          Lexing.pos_fname=_; Lexing.pos_cnum=c1} ->
        eprintf "@[%d:%d: parse error@." line (c1-c0+1))
    | Tc.Error e -> eprintf "@[%s (typecheck)@." e

(* }}} *)
