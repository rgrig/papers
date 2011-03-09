open Ast

exception Error of string

type type_ =
    Class of string
  | Method of type_ list * type_
  | Bool

let hash_globals _ =  failwith "todo"
let hash_classes _ = failwith "todo"

let add_declarations _ _ = failwith "todo"

let add_fields h (_, m) =
  let f acc = function
    | Field d -> d :: acc
    | _ -> acc in
  let d = List.fold_left f [] m in
  add_declarations h (List.rev d)

let assert_types_match t1 t2 =
  if t1 <> t2 then raise (Error "type mismatch"); t1

let rec expression _ _ _= failwith "todo"

and statement v c = 
  let expression = expression v c in
  let body = body v c in
  function
  | Return e -> Some (expression e)
  | Assignment (s, e) ->
      ignore (assert_types_match (expression (Ref s)) (expression e)); None
  | Call
      { call_lhs = _
      ; call_receiver = _
      ; call_method = _
      ; call_arguments = _ }
      -> failwith "todo"
  | Allocate s -> ignore (expression (Ref s)); None
  | While 
      { while_pre_body = b1
      ; while_condition = e
      ; while_post_body = b2 }
      ->
        ignore (assert_types_match (expression e) Bool);
        assert_types_match (body b1) (body b2)
  | If (e, b) -> 
      ignore (assert_types_match (expression e) Bool); body b

and body v c (Body (d, s)) =
  let v = add_fields v c in
  let v = add_declarations v d in
  let ts = List.map (statement v c) s in
  let f t1 t2 = match (t1, t2) with
    | (None, t) | (t, None) -> t
    | (t, _) -> t in
  List.fold_left f None ts

let program
  { program_globals = g
  ; program_classes = c
  ; program_main = m }
=
  let g = hash_globals g in
  let c = hash_classes c in
  body g c m
