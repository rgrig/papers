open Ast

exception Error of string

let rec map_find f = function
  | x :: xs -> (match f x with (Some _ as x) -> x | None -> map_find f xs)
  | [] -> None

type type_ =
    Class of string
  | Method of type_ list * type_
  | Bool

module StringMap = Map.Make (String)

module Variables = struct
  let empty = StringMap.empty
  let add vs ds =
    let f vs { declaration_type=t; declaration_variable=v } = 
      StringMap.add v t vs in
    List.fold_left f vs ds
  let lookup vs v =
    try (match (StringMap.find v vs) with
      | "bool" -> Bool
      | s -> Class s)
    with Not_found -> raise (Error ("undefined variable " ^ v))
end

module Classes = struct
  let empty = failwith "Classes.empty"
  let add cs ds = failwith "Classes.add"
  let lookup_field cs t f = failwith "Classes.lookup_field"
  let lookup_method cs t m = failwith "Classes.lookup_method"
end

let assert_types_match t1 t2 =
  if t1 <> t2 then raise (Error "type mismatch")

let call lm expression
  { call_lhs = l
  ; call_receiver = r
  ; call_method = m
  ; call_arguments = a }
=
  let tr = expression r in 
  let tmr, tma = lm tr m in
  let ta = List.map expression a in
  (try List.iter2 assert_types_match tma ta
  with Invalid_argument _ -> raise (Error "wrong number of args"));
  (match l with 
    | Some l -> assert_types_match tr (expression (Ref l)) 
    | _ -> ());
  None

let while_ b e
  { while_pre_body = b1
  ; while_condition = c
  ; while_post_body = b2 }
=
  let t1 = b b1 in let t2 = b b2 in
  assert_types_match (e c) Bool;
  assert_types_match t1 t2; t1

let rec expression ve ce =
  let expression = expression ve ce in
  function
  | Ac (_, es) ->
      let ts = List.map expression es in
      List.iter (assert_types_match Bool) ts; Bool
  | Bin (l, o, r) -> assert_types_match (expression l) (expression r); Bool
  | Not e -> assert_types_match (expression e) Bool; Bool
  | Deref (e, f) -> let te = expression e in Classes.lookup_field ce te f
  | Ref s -> Variables.lookup ve s

and statement ve ce = 
  let expression = expression ve ce in
  let body = body ve ce in
  function
  | Return e -> Some (expression e)
  | Assignment (s, e) -> 
      assert_types_match (expression (Ref s)) (expression e); None
  | Call c -> call (Classes.lookup_method ce) expression c
  | Allocate s -> ignore (expression (Ref s)); None
  | While w -> while_ body expression w
  | If (e, b) -> 
      assert_types_match (expression e) Bool; body b

and body ve ce (Body (d, s)) =
  let ve = Variables.add ve (field_declarations ce) in
  let ve = Variables.add ve d in
  let statement = statement ve ce in
  map_find statement s

let program
  { program_globals = v
  ; program_classes = c
  ; program_main = m }
=
  let ve = Variables.add Variables.empty v in
  let ce = Classes.add Classes.empty c in
  body ve ce m
