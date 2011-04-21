open Format

type value = int
type variable = string

(* common parts at the bottom: expressions mostly *) (* {{{ *)
type binop = Eq | Ne
type acop = Or | And

type 'a with_line = { ast : 'a; line : int }

type type_ =
  | Class of string
  | Bool
  | Unit
  | AnyType (* used while typechecking polymorphic literals *)

(* Expressions have no side-effects, so they don't include method calls. *)
type expression =
    Ac of acop * expression list
  | Bin of expression * binop * expression
  | Not of expression
  | Deref of expression * variable
  | Ref of variable
  | Literal of value option

(* }}} *)
(* parts used only by programs. *) (* {{{ *)

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
(* parts used only by properties *) (* {{{ *)
module Property = struct

  (* Since there's another [expression], you're asking for trouble if you
     open this module. *)
  type expression =
      Constant of value
    | Pattern of variable option
    | Guard of variable

  (* 
    Compare with [call_statement] above.
    The first argument is teh receiver.
   *)
  type label =
    { label_result : expression
    ; label_method : string
    ; label_arguments : expression list }

  type edge =
    { edge_source : string
    ; edge_target : string
    ; edge_label : label }

  type t =
    { message : string
    ; edges: edge list }

end
(* }}} *)
(* top level common parts: the big list corresponding to common.mly *) (* {{{ *)

type program =
  { program_globals : declaration list
  ; program_classes : class_ list
  ; program_main : body option 
  ; program_properties : Property.t list }

(* }}} *)
(* utilities *) (* {{{ *)

let ok_automaton =
  { Property.message = 
      "internal error: ok_automaton should be happy with all programs"
  ; Property.edges = [] }

let mk_allocate v = Allocate { allocate_lhs = v; allocate_type = None }
let mk_call l r m a = Call
  { call_lhs = l
  ; call_receiver = r
  ; call_class = None
  ; call_method = m
  ; call_arguments = a }

let default_body line = 
  Body ([], [{ ast = Return(Literal None); line = line }])
let empty_body = Body ([], [])

let rec pp_type ppf = function
  | Class n -> fprintf ppf "%s" n
  | Bool -> fprintf ppf "[Bool]"
  | Unit -> fprintf ppf "[Unit]"
  | AnyType -> fprintf ppf "[*]"

(* }}} *)
