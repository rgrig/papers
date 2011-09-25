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

type method_ = string * int

type event_type =
  | Call
  | Return

type event_tag = event_type * method_

type atomic_guard =
  | Var of variable * int
  | Ct of value * int
  | Event of event_tag
  | Any

(* TODO: perhaps go back to the representation (tag, values check), since we
         don't do desugaring to unit transitions anyway. *)
type guard =
  | Atomic of atomic_guard
  | Not of guard
  | And of guard list
  | Or of guard list

type action = (variable * int) list

type 'a event =
  { event_tag : event_tag
  ; event_values : 'a U.IntMap.t }

type label =
  { label_guard : guard
  ; label_action : action }

type edge =
  { edge_source : vertex
  ; edge_target : vertex
  ; edge_labels : label list }

type t =
  { message : string
  ; edges: edge list }
(* }}} *)
(* utilities *) (* {{{ *)
let wvars { label_guard = _; label_action = a } =
  List.map fst a

let rvars { label_guard = g; label_action = _ } =
  let rec f = function
    | Atomic (Var (v, _)) -> [v]
    | Not g -> f g
    | And gs | Or gs -> List.concat (List.map f gs)
    | _ -> [] in
  f g

let vars_of_edge f
  { edge_source = _
  ; edge_target = _
  ; edge_labels = ls }
=
  List.concat (List.map f ls)

let written_vars = vars_of_edge wvars
let read_vars = vars_of_edge rvars

let mk_event et m vs =
  { event_tag = et, m
  ; event_values =
      let f (i, acc) v = (succ i, U.IntMap.add i v acc) in
      snd (List.fold_left f (0, U.IntMap.empty) vs) }

let edge_length e = List.length e.edge_labels

let outgoing a src =
  List.filter (fun e -> e.edge_source = src) a.edges

let guards_of_automaton {message=_; edges=edges} =
  let gol acc {label_guard=g; label_action=_} = g :: acc in
  let goe acc {edge_source=_; edge_target=_; edge_labels=ls} =
    List.fold_left gol acc ls in
  List.fold_left goe [] edges

let rec push_not_down p = function
  | Atomic _ as g -> if p then g else Not g
  | Not g -> push_not_down (not p) g
  | And gs ->
      let gs = List.map (push_not_down p) gs in
      if p then And gs else Or gs
  | Or gs ->
      let gs = List.map (push_not_down p) gs in
      if p then Or gs else And gs

type conflict_var =
  | CV_ev_var of int
  | CV_aut_var of variable
  | CV_const of value

let satisfiable_term t = (* TODO: what about Atomic Event? *)
  let is_falsity = function
    | Not (Atomic Any) -> true
    | _ -> false in
  if List.exists is_falsity t then false else
  let uf = UnionFind.make () in
  let process_eq = function
    | Atomic (Var (x, i)) -> UnionFind.union uf (CV_aut_var x) (CV_ev_var i)
    | Atomic (Ct (v, i)) -> UnionFind.union uf (CV_const v) (CV_ev_var i)
    | _ -> () in
  List.iter process_eq t;
  let unsat_diseq = function
    | Not (Atomic (Var (x, i))) -> UnionFind.equals uf (CV_aut_var x) (CV_ev_var i)
    | Not (Atomic (Ct (v, i))) -> UnionFind.equals uf (CV_const v) (CV_ev_var i)
    | _ -> false in
  not (List.exists unsat_diseq t)

let simplify_term xs =
  let xs = U.unique xs in
  if (satisfiable_term xs) then Some xs else None

let simplify xss =
  let xss = U.map_option simplify_term xss in
  U.unique xss

let dnf f =
  let rec fold g = function
    | ((Atomic _) | Not (Atomic _)) as x -> List.map (U.cons x) g
    | And hs -> List.fold_left fold g hs
    | Or hs -> List.concat (List.map (fold g) hs)
    | _ -> failwith "I only work if not is pushed down" in
  simplify (fold [[]] (push_not_down true f))

let ok_automaton =
  { message =
      "internal error: ok_automaton should be happy with all programs"
  ; edges = [] }

(* }}} *)
