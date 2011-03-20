type binop = Eq | Ne
type acop = Or | And

type 'a with_line = { ast : 'a; line : int }

(* Expressions have no side-effects, so they don't include method calls. *)
type expression =
    Ac of acop * expression list
  | Bin of expression * binop * expression
  | Not of expression
  | Deref of expression * string
  | Ref of string

type declaration =
  { declaration_type : string
  ; declaration_variable : string }

type call = 
  { call_lhs : string option
  ; call_receiver : expression
  ; call_method : string
  ; call_arguments : expression list }

type statement =
    Return of expression
  | Assignment of string * expression
  | Call of call
  | Allocate of string
  | While of while_
  | If of expression * body

and while_ =
  { while_pre_body : body
  ; while_condition : expression
  ; while_post_body : body }

and body = Body of declaration list * statement with_line list

type method_ =
  { method_return_type : string
  ; method_name : string
  ; method_formals : declaration list
  ; method_body : body option }

type member =
    Field of declaration
  | Method of method_

type class_ = string * member list

type program =
  { program_globals : declaration list
  ; program_classes : class_ list
  ; program_main : body }

(* utilities *) (* {{{ *)

let empty_body = Body ([], [])

(* }}} *)
