(*
  For each type [Ast.t] there's a function [t : Ast.t-> type_].  Statements
  other than return have the type [Unit]. Composed statements, including lists,
  inherit the type of the first return they reach, and are [Unit] otherwise.

  Functions that help manipulate the typing environment are packed in
  modules.
 *)

open Ast
open Format

exception Error of string

let fail p c s = raise (Error (p ^ ": " ^ c ^ ": " ^ s))

(* [map_find d f p xs] applies [f] to each [x] and returns the first
  result that satisfies [p]. Otherwise returns the default [d]. *)
let rec map_find d f p = function
  | x :: xs -> let r = f x in if p r then r else map_find d f p xs
  | [] -> d
let map_find_not d f xs = map_find d f ((<>) d) xs

type type_ =
    Class of string
  | Method of type_ list * type_
  | Bool
  | Unit

let type_of_string = function
  | "Bool" -> Bool
  | "Unit" -> Unit
  | x -> Class x

let rec pp_list pp ppf = function
  | [] -> ()
  | [x] -> pp ppf x
  | x :: xs -> pp ppf x; fprintf ppf ",@ "; pp_list pp ppf xs

let rec pp_type ppf = function
  | Class n -> fprintf ppf "%s" n
  | Method (args, r) -> 
      fprintf ppf "%a -> %a" (pp_list pp_type) args pp_type r
  | Bool -> fprintf ppf "[Bool]"
  | Unit -> fprintf ppf "[Unit]"

(* NOTE: Fields and methods live in different namespaces. *)
module type EnvironmentT = sig
  type t
  val make : program -> t
  val update_line : t -> int -> t
  val add_variables : t -> declaration list -> t
  val position : t -> string
  val lookup_variable : t -> string -> type_
  val lookup_field : t -> type_ -> string -> type_
  val lookup_method : t -> type_ -> string -> (type_ * type_ list)
end

module Environment : EnvironmentT = struct (* {{{ *)
  module StringMap = Map.Make (String)
  
  type t = 
    { variables : type_ StringMap.t
    ; fields_by_class : type_ StringMap.t StringMap.t
    ; methods_by_class : (type_ * type_ list) StringMap.t StringMap.t 
    ; line : int option }

  let add_var m { declaration_variable = v; declaration_type = t } =
    StringMap.add v (type_of_string t) m

  let add_vars = List.fold_left add_var

  let add_declarations m ds =
    let add_one m { declaration_variable = v; declaration_type = t } =
      StringMap.add v (type_of_string t) m in
    List.fold_left add_one m ds

  let process_member (fs, ms) = function
    | Field d -> (add_var fs d, ms)
    | Ast.Method 
        { method_return_type = r
        ; method_name = n
        ; method_formals = args
        ; method_body = _ }
      ->
        let gt x = type_of_string x.declaration_type in
        let args = List.map gt args in
        let r = type_of_string r in
        (fs, StringMap.add n (r, args) ms)

  let process_class (fbc, mbc) (cn, d) =
    let fs, ms = StringMap.empty, StringMap.empty in
    let fs, ms = List.fold_left process_member (fs, ms) d in
    (StringMap.add cn fs fbc, StringMap.add cn ms mbc)

  let make { program_globals=gs; program_classes=cs; program_main=_ } =
    let fbc, mbc = StringMap.empty, StringMap.empty in
    let fbc, mbc = List.fold_left process_class (fbc, mbc) cs in
    { variables = add_vars StringMap.empty gs
    ; fields_by_class = fbc
    ; methods_by_class = mbc 
    ; line = None }

  let update_line env line = { env with line = Some line }

  let position env = match env.line with
    | None -> "?"
    | Some n -> string_of_int n

  let err p c s = fail (position p) c s

  let add_variables env d = { env with variables = add_vars env.variables d }

  let lookup_variable env id =
    try StringMap.find id env.variables
    with Not_found -> err env "undefined" id

  let get_class_info e m t =
    let n = match t with 
      | Class n -> n 
      | _ -> err e "not a class" "?" in
    try StringMap.find n m
    with Not_found -> err e "undefined class" n

  let lookup_method env t m =
    let ms = get_class_info env env.methods_by_class t in
    try StringMap.find m ms
    with Not_found -> err env "method not found" m

  let lookup_field env t f =
    let fs = get_class_info env env.fields_by_class t in
    try StringMap.find f fs
    with Not_found -> err env "field not found" f
end (* }}} *)

let assert_types_match env t1 t2 =
  if t1 <> t2 then
    let p = Environment.position env in
    let info = 
      fprintf str_formatter "@[%a and %a@]"
          pp_type t1 pp_type t2; flush_str_formatter () in
    fail p "type mismatch" info

let rec call env
  { call_lhs = l
  ; call_receiver = r
  ; call_method = m
  ; call_arguments = a }
=
  let expression = expression env in
  let assert_types_match = assert_types_match env in
  let tr = expression r in 
  let tmr, tma = Environment.lookup_method env tr m in
  let ta = List.map expression a in
  (try List.iter2 assert_types_match tma ta
  with Invalid_argument _ ->
    fail (Environment.position env) "wrong number of arguments" m);
  (match l with 
    | Some l -> assert_types_match tmr (expression (Ref l)) 
    | _ -> ());
  Unit

and while_ env
  { while_pre_body = b1
  ; while_condition = c
  ; while_post_body = b2 }
=
  let assert_types_match = assert_types_match env in
  let body = body env in
  let expression = expression env in
  let t1 = body b1 in let t2 = body b2 in
  assert_types_match (expression c) Bool;
  assert_types_match t1 t2; t1

and expression env =
  let expression x = expression env x in
  let assert_types_match = assert_types_match env in
  function
  | Ac (_, es) ->
      let ts = List.map expression es in
      List.iter (assert_types_match Bool) ts; Bool
  | Bin (l, o, r) -> assert_types_match (expression l) (expression r); Bool
  | Not e -> assert_types_match (expression e) Bool; Bool
  | Deref (e, f) -> Environment.lookup_field env (expression e) f
  | Ref s -> Environment.lookup_variable env s

and statement env {ast = ast; line = line} = 
  let env = Environment.update_line env line in
  let assert_types_match = assert_types_match env in
  let expression = expression env in
  let body = body env in
  let call = call env in
  match ast with
    | Return e -> expression e
    | Assignment (s, e) -> 
        assert_types_match (expression (Ref s)) (expression e); Unit
    | Call c -> call c
    | Allocate s -> ignore (expression (Ref s)); Unit
    | While w -> while_ env w
    | If (e, b) -> 
        assert_types_match (expression e) Bool; body b

and body env (Body (d, s)) =
  map_find_not Unit (statement (Environment.add_variables env d)) s

let method_ env
  { method_return_type = r
  ; method_name = n
  ; method_formals = args
  ; method_body = b }
=
  match b with
    | None -> ()
    | Some b ->
        let env = Environment.add_variables env args in
        let tr = body env b in
        assert_types_match env tr (type_of_string r)

let class_ env (_, ds) =
  let f (fs, ms) = function
    | Field f -> (f :: fs, ms)
    | Ast.Method m -> (fs, m :: ms) in
  let fs, ms = List.fold_left f ([], []) ds in
  let env = Environment.add_variables env fs in
  List.iter (method_ env) (List.rev ms)

let program p =
  let env = Environment.make p in
  List.iter (class_ env) p.program_classes;
  body env p.program_main
