(* modules *) (* {{{ *)
open Format

module U = Util

(* }}} *)

type value = int
type variable = string

(* AST types shared by programs and automata (towards leaves) *) (* {{{ *)
type binop = Eq | Ne
type acop = Or | And

type 'a with_line = { ast : 'a; line : int }

type type_ =
  | Class of string
  | Bool
  | Unit
  | AnyType of type_ option ref (* used while typechecking Nondet *)

(* Expressions have no side-effects, so they don't include method calls. *)
type expression =
    Ac of acop * expression list
  | Bin of expression * binop * expression
  | Not of expression
  | Deref of expression * variable
  | Ref of variable
  | Literal of value option * type_ option ref

(* }}} *)
(* AST (types) only for programs. *) (* {{{ *)

type declaration =
  { declaration_type : type_
  ; declaration_variable : string }

type call_statement =
  { call_lhs : string option
  ; call_receiver : expression
  ; mutable call_class : string option
  ; call_method : string
  ; call_arguments : expression list }

type allocate_statement =
  { allocate_lhs : string
  ; mutable allocate_type : type_ option }

type statement =
    Return of expression
  | Assignment of string * expression
  | Call of call_statement
  | Allocate of allocate_statement
  | While of while_
  | If of expression * body

and while_ =
  { while_pre_body : body
  ; while_condition : expression
  ; while_post_body : body }

and body = Body of declaration list * statement with_line list

type method_ =
  { method_return_type : type_
  ; method_name : string
  ; method_formals : declaration list
  ; method_body : body }

type member =
    Field of declaration
  | Method of method_

type class_ = string * member list

(* }}} *)
(* AST (types) only for automata *) (* {{{ *)
module PropertyAst = struct
  (* Say [module PA = PropertyAst] rather than [open PropertyAst] when using. *)

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

  type guard =
    | Atomic of atomic_guard
    | Not of guard
    | And of guard list
    | Or of guard list

  type action = (variable * int) list

  type event =
    { event_tag : event_tag
    ; event_values : value U.IntMap.t }

  type label =
    { label_guard : guard
    ; label_action : action }

  type edge =
    { edge_source : string
    ; edge_target : string
    ; edge_labels : label list }

  type t =
    { message : string
    ; edges: edge list }

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

  let mk_event et m (vs : value list) =
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

  let satisfiable_term t =
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

  (* }}} *)
end
(* }}} *)
(* Root of AST, see common.mly. *) (* {{{ *)

type program =
  { program_globals : declaration list
  ; program_classes : class_ list
  ; program_main : body option
  ; program_properties : PropertyAst.t with_line list }

(* }}} *)
(* utilities *) (* {{{ *)

let ok_automaton =
  { PropertyAst.message =
      "internal error: ok_automaton should be happy with all programs"
  ; PropertyAst.edges = [] }

let mk_allocate v = Allocate { allocate_lhs = v; allocate_type = None }
let mk_call l r m a = Call
  { call_lhs = l
  ; call_receiver = r
  ; call_class = None
  ; call_method = m
  ; call_arguments = a }

let default_body line =
  Body ([], [{ ast = Return(Literal (None, ref None)); line = line }])
let empty_body = Body ([], [])

let rec pp_type ppf = function
  | Class n -> fprintf ppf "%s" n
  | Bool -> fprintf ppf "[Bool]"
  | Unit -> fprintf ppf "[Unit]"
  | AnyType {contents=t} -> fprintf ppf "<%a>" (U.pp_option pp_type) t

(* }}} *)
