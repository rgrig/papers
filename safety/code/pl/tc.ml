(*
  For each type [Ast.t] there's a function [t : Ast.t-> type_].  Statements
  have the type [Unit], except return. Composed statements, including lists,
  inherit the type of the first return they reach, and are [Unit] otherwise.
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

module StringMap = Map.Make (String)

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
  type t = 
    { variables : type_ StringMap.t
    ; fields_by_class : type_ StringMap.t StringMap.t
    ; methods_by_class : (type_ * type_ list) StringMap.t StringMap.t 
    ; line : int option }

  let object_layout env = env.fields_by_class

  let position env = match env.line with
    | None -> "?"
    | Some n -> string_of_int n

  let err p c s = fail (position p) c s

  let check_type env = function
    | Bool | Unit -> ()
    | AnyType -> failwith "AnyType should not be created by parser."
    | Class c ->
        if not (StringMap.mem c env.fields_by_class) then
          err env "class not declared" c

  let check_types_exist env =
    let check_type = check_type env in
    let check_method_type (r, args) =
      check_type r; List.iter check_type args in
    let map_check check vs = StringMap.iter (fun _ t -> check t) vs in
    map_check check_type env.variables;
    map_check (map_check check_type) env.fields_by_class;
    map_check (map_check check_method_type) env.methods_by_class

  let add_var vs { declaration_variable = v; declaration_type = t } =
    StringMap.add v t vs

  let add_vars = List.fold_left add_var

  let process_member (fs, ms) = function
    | Field d -> (add_var fs d, ms)
    | Ast.Method 
        { method_return_type = r
        ; method_name = n
        ; method_formals = args
        ; method_body = _ }
      ->
        let gt x = x.declaration_type in
        let args = List.map gt args in
        (fs, StringMap.add n (r, args) ms)

  let process_class (fbc, mbc) (cn, d) =
    let fs, ms = StringMap.empty, StringMap.empty in
    let fs, ms = List.fold_left process_member (fs, ms) d in
    (StringMap.add cn fs fbc, StringMap.add cn ms mbc)

  let make { program_globals=gs; program_classes=cs; program_main=_ } =
    let fbc, mbc = StringMap.empty, StringMap.empty in
    let fbc, mbc = List.fold_left process_class (fbc, mbc) cs in
    let env = 
      { variables = add_vars StringMap.empty gs
      ; fields_by_class = fbc
      ; methods_by_class = mbc 
      ; line = None } in
    check_types_exist env;
    env

  let update_line env line = { env with line = Some line }

  let add_variables env d =
    List.iter (fun x -> check_type env x.declaration_type) d;
    { env with variables = add_vars env.variables d }

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

let check_types_match env t1 t2 =
  if t1 <> t2 && t1 <> AnyType && t2 <> AnyType then
    let p = Environment.position env in
    let info = 
      fprintf str_formatter "@[%a and %a@]"
          pp_type t1 pp_type t2; flush_str_formatter () in
    fail p "type mismatch" info

let rec call env c =
  let expression = expression env in
  let check_types_match = check_types_match env in
  let string_of_class = function
    | Class c -> c
    | _ -> fail (Environment.position env) "expected class, not primitive" "" in
  let tr = expression c.call_receiver in 
  let tmr, tma = Environment.lookup_method env tr c.call_method in
  let ta = List.map expression c.call_arguments in
  c.call_class <- Some (string_of_class tr);
  (try List.iter2 check_types_match tma ta
  with Invalid_argument _ ->
    fail (Environment.position env) "wrong number of arguments" c.call_method);
  (match c.call_lhs with 
    | Some l -> check_types_match tmr (expression (Ref l)) 
    | _ -> ());
  Unit

and while_ env
  { while_pre_body = b1
  ; while_condition = c
  ; while_post_body = b2 }
=
  let check_types_match = check_types_match env in
  let body = body env in
  let expression = expression env in
  let t1 = body b1 in let t2 = body b2 in
  check_types_match (expression c) Bool;
  check_types_match t1 t2; t1

and expression env =
  let expression x = expression env x in
  let check_types_match = check_types_match env in
  function
    | Ac (_, es) ->
        let ts = List.map expression es in
        List.iter (check_types_match Bool) ts; Bool
    | Bin (l, o, r) -> check_types_match (expression l) (expression r); Bool
    | Not e -> check_types_match (expression e) Bool; Bool
    | Deref (e, f) -> Environment.lookup_field env (expression e) f
    | Ref s -> Environment.lookup_variable env s
    | Literal _ -> AnyType

and allocate env a =
  let expression = expression env in
  let t = expression (Ref a.allocate_lhs) in
  a.allocate_type <- Some t; Unit

and statement env {ast = ast; line = line} = 
  let env = Environment.update_line env line in
  let allocate = allocate env in
  let body = body env in
  let call = call env in
  let check_types_match = check_types_match env in
  let expression = expression env in
  match ast with
    | Return e -> expression e
    | Assignment (s, e) -> 
        check_types_match (expression (Ref s)) (expression e); Unit
    | Call c -> call c
    | Allocate a -> allocate a
    | While w -> while_ env w
    | If (e, b) -> 
        check_types_match (expression e) Bool; body b

and body env (Body (d, s)) =
  map_find_not Unit (statement (Environment.add_variables env d)) s

let method_ env
  { method_return_type = r
  ; method_name = n
  ; method_formals = args
  ; method_body = b }
=
  let env = Environment.add_variables env args in
  let tr = body env b in
  check_types_match env tr r

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
