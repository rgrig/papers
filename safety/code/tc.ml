(*
  For each type [Ast.t] there's a function [t : Ast.t-> type_].  Statements
  have the type [Unit], except return. Composed statements, including lists,
  inherit the type of the first return they reach, and are [Unit] otherwise.
 *)
(* modules *) (* {{{ *)
open Ast
open Format
module U = Util

(* }}} *)
(* environment and other utilities *) (* {{{ *)

exception Error of string

let fail p c s = raise (Error (p ^ ": " ^ c ^ ": " ^ s))

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
    { variables : type_ U.StringMap.t
    ; fields_by_class : type_ U.StringMap.t U.StringMap.t
    ; methods_by_class : (type_ * type_ list) U.StringMap.t U.StringMap.t
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
        if not (U.StringMap.mem c env.fields_by_class) then
          err env "class not declared" c

  let check_types_exist env =
    let check_type = check_type env in
    let check_method_type (r, args) =
      check_type r; List.iter check_type args in
    let map_check check vs = U.StringMap.iter (fun _ t -> check t) vs in
    map_check check_type env.variables;
    map_check (map_check check_type) env.fields_by_class;
    map_check (map_check check_method_type) env.methods_by_class

  let add_var vs { declaration_variable = v; declaration_type = t } =
    U.StringMap.add v t vs

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
        (fs, U.StringMap.add n (r, args) ms)

  let process_class (fbc, mbc) (cn, d) =
    let fs, ms = U.StringMap.empty, U.StringMap.empty in
    let fs, ms = List.fold_left process_member (fs, ms) d in
    (U.StringMap.add cn fs fbc, U.StringMap.add cn ms mbc)

  let make { program_globals=gs; program_classes=cs; program_main=_ } =
    let fbc, mbc = U.StringMap.empty, U.StringMap.empty in
    let fbc, mbc = List.fold_left process_class (fbc, mbc) cs in
    let env =
      { variables = add_vars U.StringMap.empty gs
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
    try U.StringMap.find id env.variables
    with Not_found -> err env "undefined" id

  let get_class_info e m t =
    let n = match t with
      | Class n -> n
      | _ -> err e "not a class" "?" in
    try U.StringMap.find n m
    with Not_found -> err e "undefined class" n

  let lookup_method env t m =
    let ms = get_class_info env env.methods_by_class t in
    try U.StringMap.find m ms
    with Not_found -> err env "method not found" m

  let lookup_field env t f =
    let fs = get_class_info env env.fields_by_class t in
    try U.StringMap.find f fs
    with Not_found -> err env "field not found" f
end (* }}} *)

let check_types_match env t1 t2 =
  if t1 <> t2 && t1 <> AnyType && t2 <> AnyType then
    let p = Environment.position env in
    let info =
      fprintf str_formatter "@[%a and %a@]"
          pp_type t1 pp_type t2; flush_str_formatter () in
    fail p "type mismatch" info

(* }}} *)
(* typechecking of programs *) (* {{{ *)
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
  Util.map_find_not Unit (statement (Environment.add_variables env d)) s

let method_ env
  { method_return_type = r
  ; method_name = n
  ; method_formals = args
  ; method_body = b }
=
  let env = Environment.add_variables env args in
  let tr = body env b in
  check_types_match env tr r

let class_ env (c, ds) =
  let f (fs, ms) = function
    | Field f -> (f :: fs, ms)
    | Ast.Method m -> (fs, m :: ms) in
  let fs, ms = List.fold_left f ([], []) ds in
  let env = Environment.add_variables env fs in
  let env = Environment.add_variables env
    [{declaration_type=Class c; declaration_variable="this"}] in
  List.iter (method_ env) (List.rev ms)

(* }}} *)
(* static checks for properties *) (* {{{ *)

module PropertyChecks = struct
  module A = Ast.PropertyAst

  let warnings = ref []
  let location = ref "<INTERNAL ERROR>" (* user should not see this *)
  let set_location = function
    | None -> location := "?"
    | Some l -> location := sprintf "%d" l
  let warn m =
    warnings := sprintf "%s: Warning: %s" !location m :: !warnings

  let get_source e = e.A.edge_source
  let get_target e = e.A.edge_target

  let default d f m x = try f x m with Not_found -> d

  let adjacency_of_edges source target es =
    let f acc e =
      let s, t = source e, target e in
      let old = try U.StringMap.find s acc with Not_found -> [] in
      U.StringMap.add s (t :: old) acc in
    default [] U.StringMap.find (List.fold_left f U.StringMap.empty es)

  let rec reachable_from g s =
    let r = ref U.StringSet.empty in
    let rec f s =
      if not (U.StringSet.mem s !r) then begin
        r := U.StringSet.add s !r;
        List.iter f (g s)
      end in
    f s; !r

  let check_unused_states p =
    let succ = adjacency_of_edges get_source get_target p.A.edges in
    let pred = adjacency_of_edges get_target get_source p.A.edges in
    let fs = reachable_from succ "start" in
    let te = reachable_from pred "error" in
    let collect get s =
      List.fold_left (fun s e -> U.StringSet.add (get e) s) s p.A.edges in
    let all = collect get_target (collect get_source U.StringSet.empty) in
    let bad = U.StringSet.diff all (U.StringSet.inter fs te) in
    if not (U.StringSet.mem "start" all) then warn "missing start";
    if not (U.StringSet.mem "error" all) then warn "missing error";
    U.StringSet.iter (fun s -> warn (sprintf "unused state: %s" s)) bad

  let warn_bad_edge msg e vs = match U.StringSet.elements vs with
    | [] -> ()
    | vs ->
        fprintf str_formatter "%s->%s: " e.A.edge_source e.A.edge_target;
        fprintf str_formatter "%s: " msg;
        U.pp_list ", " U.pp_s str_formatter vs;
        warn (flush_str_formatter ())

  let check_linear_patterns p =
    let check_edge e =
      let see (seen, bad) x =
        if U.StringSet.mem x seen
        then (seen, U.StringSet.add x bad)
        else (U.StringSet.add x seen, bad) in
      let ps = A.patterns e in
      let _, vs = List.fold_left see (U.StringSet.empty, U.StringSet.empty) ps in
      warn_bad_edge "multiple bindings" e vs in
    List.iter check_edge p.A.edges

  let bindings =
    let f _ e =
      let ps = A.patterns e in
      List.fold_left (U.flip U.StringSet.add) U.StringSet.empty ps in
    U.y (U.memo f)

  let dump_set s =
    eprintf "@[";
    U.pp_list ", " U.pp_s err_formatter (U.StringSet.elements s);
    eprintf "@]"

  (*
    Associates a value with each state in property [p]. The property is seen as
    a graph, [vf] and [ef] are applied repeatedly to update values associated to
    vertices and edges until a fixpoint is reached. The function [vf] takes the
    values on incoming edges, a vertex, and returns a vertex value; the function
    [ef] takes the value on the source vertex, an edge, and returns an edge
    value. The value [d] is the initial value tacked on vertices.
   *)
  let fixpoint_on_graph p d vf ef =
    let incoming = adjacency_of_edges get_target (fun e->e) p.A.edges in
    let outgoing = adjacency_of_edges get_source (fun e->e) p.A.edges in
    let ve = Hashtbl.create 13 in (* cache for edge values *)
    let rec loop vv dv =
      if U.StringSet.is_empty dv then vv else begin
        let ef e =
          try Hashtbl.find ve e
          with Not_found ->
            let vf v = try U.StringMap.find v vv with Not_found -> d in
            let r = ef (vf (get_source e)) e in
            Hashtbl.add ve e r; r in
        let v = U.StringSet.choose dv in
        let dv = U.StringSet.remove v dv in
        let w = vf (List.map ef (incoming v)) v in
        let edv =
          if not (U.StringMap.mem v vv) || U.StringMap.find v vv <> w then begin
            let es = outgoing v in
            List.iter (fun e -> Hashtbl.remove ve e) es;
            List.map get_target es
          end else [] in
        loop (U.StringMap.add v w vv) (List.fold_left (U.flip U.StringSet.add) dv edv)
      end in
    let m = loop U.StringMap.empty (U.StringSet.singleton "start") in
    default d U.StringMap.find m

  let bound_variables p =
    let v_k es_k v = match es_k, v with
      | _, "start" | [], _ -> U.StringSet.empty
      | e::ev, _ -> List.fold_left U.StringSet.inter e ev in
    let e_k s_k e = U.StringSet.union s_k (bindings e) in
    fixpoint_on_graph p U.StringSet.empty v_k e_k

  let check_bound_variables p =
    let bound = bound_variables p in
    let check_edge e =
      let guards = A.guards e in
      let guards = List.fold_left (U.flip U.StringSet.add) U.StringSet.empty guards in
      let bound_here = bound (get_source e) in
      warn_bad_edge "possibly unbound" e (U.StringSet.diff guards bound_here) in
    List.iter check_edge p.A.edges

  let all p =
    set_location (Some p.line);
    let p = p.ast in
    check_unused_states p;
    check_linear_patterns p;
    check_bound_variables p
end

(* }}} *)
let program p =
  let env = Environment.make p in
  List.iter (class_ env) p.program_classes;
  PropertyChecks.warnings := [];
  List.iter PropertyChecks.all p.program_properties;
  (match p.program_main with
    | None -> ()
    | Some m -> ignore (body env m));
  List.rev !PropertyChecks.warnings
