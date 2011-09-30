(* modules *) (* {{{ *)
open Format

module U = Util

(* }}} *)
(* data types *) (* {{{ *)

type 'a with_line = { ast : 'a; line : int }

type value = string

type variable = string

type vertex = string

(*
 'a denotes variable
 'b denotes value
*)
type ('a, 'b) value_guard =
  | Variable of 'a * int
  | Constant of 'b * int

type event_type =
  | Call
  | Return

type ('a, 'b, 'c) tag =
  { event_type : 'a
  ; method_name : 'b
  ; method_arity : 'c }

type 'a tag_guard = ((event_type option), 'a, (int option)) tag

type event_tag = (event_type, string, int) tag

type ('a, 'b) event_guard =
  { tag_guard : 'a tag_guard
  ; value_guards : 'b list }

let check_event_guard g =
  let chk n = function Variable (_, m) | Constant (_, m) ->
    assert (0 <= m && m < n) in
  let chk_all m = List.iter (chk m) g.value_guards in
  U.option () chk_all g.tag_guard.method_arity

(* 'a denotes variable *)
type 'a action = ('a * int) list

type 'a event =
  { event_tag : event_tag
  ; event_values : 'a U.IntMap.t }
  (* I'm using an IntMap rather than an array because I prefer immutability.
    Performance is unlikely to be a problem as the typical size is <5. *)

type ('a, 'b, 'c) label =
  { guard : ('a, 'b) event_guard
  ; action : 'c action }
  (* TODO 'b and 'c relation *)

type ('a, 'b, 'c) transition =
  { source : vertex
  ; target : vertex
  ; labels : ('a, ('b, 'c) value_guard, 'b) label list }

type t =
  { name : string
  ; message : string
  ; transitions: (Str.regexp, variable, value) transition list }
(* }}} *)
(* utilities *) (* {{{ *)
let wvars l =
  List.map fst l.action

let rvars l =
  let f = function Variable (x, _) -> Some x | _ -> None in
  U.map_option f l.guard.value_guards

let vars_of_edge f e =
  List.concat (List.map f e.labels)

let written_vars t = vars_of_edge wvars t
let read_vars t = vars_of_edge rvars t

let mk_event et mn ma vs =
  { event_tag =
    { event_type = et
    ; method_name = mn
    ; method_arity = ma }
  ; event_values =
      let f (i, acc) v = (succ i, U.IntMap.add i v acc) in
      snd (List.fold_left f (0, U.IntMap.empty) vs) }

let mk_event_guard t v =
  let r = { tag_guard = t; value_guards = v } in
  check_event_guard r; r

let edge_length e = List.length e.labels

let outgoing a src =
  List.filter (fun e -> e.source = src) a.transitions

let guards_of_automaton {transitions=ts; _ } =
  let gol acc l = l.guard :: acc in
  let goe acc e = List.fold_left gol acc e.labels in
  List.fold_left goe [] ts

let get_tag_guards p =
  let gs = guards_of_automaton p in
  List.map (fun x -> x.tag_guard) gs

let get_value_guards p =
  let gs = guards_of_automaton p in
  List.concat (List.map (fun x -> x.value_guards) gs)

let ok_automaton =
  { name = "AlwaysOk"
  ; message =
      "internal error: ok_automaton should be happy with all programs"
  ; transitions = [] }

(* }}} *)
