(* modules *) (* {{{ *)
open Format

module U = Util

(* }}} *)
(* data types *) (* {{{ *)
type 'a with_line = { ast : 'a; line : int }

type value = string
  (* for Java, an expression that builds a value;
     for SOOL, an integer *)

type variable = string

type vertex = string

type value_guard =
  | Variable of variable * int
  | Constant of value * int

type event_type =
  | Call
  | Return

type ('a, 'b, 'c) tag =
  { event_type : 'a
  ; method_name : 'b
  ; method_arity : 'c }

type tag_guard = ((event_type option), Str.regexp, (int option)) tag

type event_tag = (event_type, string, int) tag

type event_guard =
  { tag_guard : tag_guard
  ; value_guards : value_guard list }

let check_event_guard g =
  let chk n = function Variable (_, m) | Constant (_, m) ->
    assert (0 <= m && m < n) in
  let chk_all m = List.iter (chk m) g.value_guards in
  U.option () chk_all g.tag_guard.method_arity

type action = (variable * int) list

type 'a event =
  { event_tag : event_tag
  ; event_values : 'a U.IntMap.t }

type label =
  { guard : event_guard
  ; action : action }

type transition =
  { source : vertex
  ; target : vertex
  ; labels : label list }

type t =
  { name : string
  ; message : string
  ; transitions: transition list }
(* }}} *)
(* utilities *) (* {{{ *)
let wvars l =
  List.map fst l.action

let rvars l =
  let f = function Variable (x, _) -> Some x | _ -> None in
  U.map_option f l.guard.value_guards

let vars_of_edge f e =
  List.concat (List.map f e.labels)

let written_vars = vars_of_edge wvars
let read_vars = vars_of_edge rvars

let mk_event et mn ma vs =
  { event_tag =
    { event_type = et
    ; method_name = mn
    ; method_arity = ma }
  ; event_values =
      let f (i, acc) v = (succ i, U.IntMap.add i v acc) in
      snd (List.fold_left f (0, U.IntMap.empty) vs) }

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

type conflict_var =
  | CV_ev_var of int
  | CV_aut_var of variable
  | CV_const of value

let ok_automaton =
  { name = "AlwaysOk"
  ; message =
      "internal error: ok_automaton should be happy with all programs"
  ; transitions = [] }

(* }}} *)
(* TODO
    - variable and value should be parameters of a functor?
    - change [event_values] to an array?
 *)
