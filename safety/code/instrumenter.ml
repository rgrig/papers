(* modules *) (* {{{ *)
module A = Ast
module B = BaristaLibrary
module PA = Ast.PropertyAst
open Format
open Util
(* }}} *)
(* representation of automata in Java *) (* {{{ *)

type vertex = int
type variable = int
type value = string (* Java literal *)

(* TODO *)
type tag =
  { pattern : string (* Java regular expression *)
  ; arity : int }

type atomic_condition =
  | AC_var of variable * int
  | AC_ct of value * int

type condition = atomic_condition list

type action = (variable * int) list

type step =
  { tag : tag
  ; condition : condition
  ; action : action }

type transition =
  { steps : step list
  ; target : vertex }

type automaton =
  { starts : vertex list
  ; errors : vertex list
  ; adjacency : transition array }

(* }}} *)

(* Things that are integers in Java (for efficiency). *)
let vertex = Hashtbl.create 31
let variable = Hashtbl.create 13
let tag = Hashtbl.create 13

let to_ints h xs =
  let c = ref (-1) in
  Hashtbl.clear h;
  let f x = if not (Hashtbl.mem h x) then (incr c; Hashtbl.add h x !c) in
  List.iter f xs

let get_vertices p =
  let f acc {PA.edge_source=s;PA.edge_target=t;PA.edge_labels=_} =
    s :: t :: acc in
  "start" :: "error" :: List.fold_left f [] p.PA.edges

let rec atomics_of_guard = function
  | PA.Atomic a -> [a]
  | PA.Not g -> atomics_of_guard g
  | PA.And gs | PA.Or gs -> List.concat (List.map atomics_of_guard gs)

let get_atomics p =
  let gs = PA.guards_of_automaton p in
  List.concat (List.map atomics_of_guard gs)

let get_variables p =
  let f = function PA.Var (v, _) -> Some v | _ -> None in
  map_option f (get_atomics p)

let get_tags p =
  let f = function PA.Event t -> Some t | _ -> None in
  map_option f (get_atomics p)

type adjacency_list = ((PA.label list * int) list) list

let transform_graph : PA.t list -> adjacency_list = fun es -> todo ()

(*
  let a = Array.make (Hashtbl.length vertex) [] in
  let f {PA.edge_source=s;PA.edge_target=t;PA.edge_labels=ls} =
    let s = Hashtbl.find vertex s in
    let t = Hashtbl.find vertex t in
    a.(s) <- (ls, t) :: a.(s) in
  List.iter f es;
  Array.to_list a
*)

let transform_guard g =
  let g = PA.dnf g in
  let split (t, gs) = function
    | PA.Not (PA.Atomic (PA.Event _)) -> failwith "Can't express in Java (1)"
    | PA.Atomic (PA.Event t') -> assert (t = None); Some t', gs
    | g -> t, g :: gs in
  let g = List.map (List.fold_left split (None, [])) g in
  let t, gs = List.split g in
  let t = map_option (fun x -> x) t in
  let gs = PA.simplify gs in
  match gs with
    | _ :: _ :: _ -> failwith "Can't express in Java (2)"
    | [] -> [], PA.Not (PA.Atomic PA.Any)
    | [gs] -> List.map (Hashtbl.find tag) t, PA.And gs

let id = fresh_id ()

let rec pp_list pe ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "@\n%a" pe x
  | x :: xs -> fprintf ppf "@\n%a,%a" pe x (pp_list pe) xs

let rec guard ppf = function
  | PA.Atomic (PA.Var (av, ev)) -> fprintf ppf "new Checker.StoreEqualityGuard(%d, %d)" ev (Hashtbl.find variable av)
  | PA.Atomic (PA.Ct (ct, ev)) -> fprintf ppf "new Checker.ConstantEqualityGuard(%d, %d)" ev ct
  | PA.Atomic (PA.Any) -> fprintf ppf "new Checker.TrueGuard()"
  | PA.Atomic (PA.Event _) -> failwith "INTERNAL: should be pruned by transform_guard"
  | PA.Not g -> fprintf ppf "new Checker.NotGuard(%a)" guard g
  | PA.And [] -> guard ppf (PA.Atomic PA.Any)
  | PA.Or [] -> guard ppf (PA.Not (PA.Atomic PA.Any))
  | PA.And [g] | PA.Or [g] -> guard ppf g
  | PA.And gs -> fprintf ppf "@[<2>new Checker.AndGuard(new Checker.Guard[]{%a})@]" (pp_list guard) gs
  | PA.Or gs -> fprintf ppf "@[<2>new Checker.OrGuard(new Checker.Guard[]{%a})@]" (pp_list guard) gs

let assignment ppf (av, ev) =
  fprintf ppf "new Checker.Action.Assignment(%d, %d)"
    (Hashtbl.find variable av) ev

let action ppf xs =
  fprintf ppf "@[<2>new Checker.Action(new Checker.Action.Assignment[]{%a})@]" (pp_list assignment) xs

let label ppf {PA.label_guard=g; PA.label_action=a} =
  let gt, gr = transform_guard g in
  fprintf ppf "@[<2>new Checker.TransitionStep(";
  fprintf ppf   "@\n@[<2>new int[]{%a}@],"
    (pp_list (fun ppf x -> fprintf ppf "%d" x)) gt;
  fprintf ppf   "@\n%a," guard gr;
  fprintf ppf   "@\n%a)@]" action a

let one_outgoing ppf (ls, tgt) =
  fprintf ppf "@[<2>new Checker.Transition(@\n@[<2>new Checker.TransitionStep[]{%a}@],@\n%d)@]"
    (pp_list label) ls
    tgt

let all_outgoing ppf ts =
  fprintf ppf "@[<2>new Checker.Transition[]{%a}@]" (pp_list one_outgoing) ts

let property ppf p = todo () (*
  to_ints vertex (get_vertices p);
  to_ints variable (get_variables p);
  to_ints tag (get_tags p);
  fprintf ppf "@[<2>";
  fprintf ppf "Checker %s = new Checker(" (id ());
  fprintf ppf   "@\n\"%s\"," p.PA.message;
  fprintf ppf   "@\n@[<2>new Checker.Automaton(%d, %d, new Checker.Transition[][]{%a})@]"
    (Hashtbl.find vertex "start")
    (Hashtbl.find vertex "error")
    (pp_list all_outgoing) (transform_graph p.PA.edges);
  fprintf ppf ");";
  fprintf ppf "@]@\n"
  (* TODO: print [vertex], [variable], [tag] (in comments). *)
*)

let file f =
  try
    printf "// from file %s@\n" f;
    let f = Helper.parse f in
    let ps = List.map (fun x -> x.A.ast) f.A.program_properties in
    List.iter (property std_formatter) ps
  with Helper.Parsing_failed m ->
    eprintf "@[%s@." m

let print_list xs =
  printf "@[";
  List.iter (fun x -> printf "%s\n" x) xs;
  printf "@."

(* TODO(rgrig): Get the classpath treatement from friendly_cli in jStar. *)
let classpath () =
  try
    "CLASSPATH"
    >> Sys.getenv
    >> Str.split (Str.regexp ":")
  with Not_found ->
    eprintf "@[Please set CLASSPATH.@."; []

let endswith suffix s =
  let m = String.length suffix in
  let n = String.length s in
  if m > n then false else String.sub s (n - m) m = suffix

let classfiles_of_path =
  fs_filter (endswith ".class")

(*
let classes_of_lowlevel_classes _ = failwith "todo"
*)

let classes_of_classfile fn =
  try fn
    >> open_in
    >> B.InputStream.make_of_channel
    >> B.ClassFile.read
    >> B.ClassDefinition.decode
    >> (fun x -> [(x, fn)])
  with
  | B.InputStream.Exception e ->
      eprintf "@[%s: %s@." fn (B.InputStream.string_of_error e); []
  | _ ->
      eprintf "@[%s: error@." fn; []

(*
let name_of_method = function
  | B.Method.Regular r ->
      [r.B.Method.name >> B.Name.utf8_for_method >> B.Utils.UTF8.to_string]
  | _ -> []
*)

let utf8 = B.Utils.UTF8.of_string
let utf8_for_class x = B.Name.make_for_class_from_external (utf8 x)
let utf8_for_field x = B.Name.make_for_field (utf8 x)
let utf8_for_method x = B.Name.make_for_method (utf8 x)
let java_lang_Object = utf8_for_class "java.lang.Object"
let java_lang_System = utf8_for_class "java.lang.System"
let java_lang_String = utf8_for_class "java.lang.String"
let java_io_PrintStream = utf8_for_class "java.io.PrintStream"
let out = utf8_for_field "out"
let println = utf8_for_method "println"
let event = utf8_for_class "topl.Checker$Event"
(* let event_init = utf8_for_method "topl.Checker$Event.<init>" *)
let init = utf8_for_method "<init>"
let checker = utf8_for_class "topl.Checker"
let check = utf8_for_method "check"

let bc_print_utf8 us = [
  B.Instruction.GETSTATIC (java_lang_System, out, `Class java_io_PrintStream);
  B.Instruction.LDC (`String us);
  B.Instruction.INVOKEVIRTUAL (`Class_or_interface java_io_PrintStream,
			     println,
			     ([`Class java_lang_String], `Void));
]
let bc_print s = bc_print_utf8 (utf8 s)
let bc_print_par p = bc_print_utf8 (p.B.Signature.identifier)

let bc_push i =
  if i = 0 then B.Instruction.ICONST_0 else
  if i = 1 then B.Instruction.ICONST_1 else
  if i = 2 then B.Instruction.ICONST_2 else
  if i = 3 then B.Instruction.ICONST_3 else
  if i = 4 then B.Instruction.ICONST_4 else
  if i = 5 then B.Instruction.ICONST_5 else
    B.Instruction.LDC (`Int (Int32.of_int i))

let bc_aload i = B.Instruction.ALOAD (B.Utils.u1 i)

let bc_new_object_array size =
  [
    bc_push size;
    B.Instruction.ANEWARRAY (`Class_or_interface java_lang_Object)
  ]

let bc_array_set for_static index =
  [
    B.Instruction.DUP;
    bc_push index;
    bc_aload (index + (if for_static then 0 else 1));
    B.Instruction.AASTORE
  ]

let bc_new_event id =
  [
    B.Instruction.NEW event;
    B.Instruction.DUP_X1;
    B.Instruction.SWAP;
    bc_push id;
    B.Instruction.SWAP;
    B.Instruction.INVOKESPECIAL (event,
			       init,
			       ([`Int; `Array (`Class java_lang_Object)], `Void)
			      )
  ]

let bc_check =
  [
    B.Instruction.ACONST_NULL; (* This should be a reference to the checker *)
    B.Instruction.SWAP;
    B.Instruction.INVOKEVIRTUAL (`Class_or_interface checker,
			       check,
			       ([`Class event], `Void)
			      )
  ]

let id_for_method s r = Hashtbl.hash (s, r)

let bc_send_event method_name desc is_static =
  let param_types, return = desc in
(*
  let obj_arr = "values" in
*)
  let fold (instructions, i) _ =
    ((bc_array_set is_static i) :: instructions, succ i) in
  let (inst_lists, _) = List.fold_left fold ([], 0) param_types in
  let instructions = List.flatten (List.rev inst_lists) in
  let id = id_for_method method_name return in
    (bc_new_object_array (List.length param_types)) @
    instructions @
    (bc_new_event id) @
    bc_check

(* Taken from disassembler.ml *)
let (++) = B.UTF8Impl.(++)
let space = B.UTF8Impl.of_string " "
let comma = B.UTF8Impl.of_string ","
let opening_parenthesis = B.UTF8Impl.of_string "("
let closing_parenthesis = B.UTF8Impl.of_string ")"
let utf8_of_method_desc name desc =
  let params, return = desc in
  (B.Descriptor.external_utf8_of_java_type return)
    ++ space
    ++ (B.Name.utf8_for_method name)
    ++ opening_parenthesis
    ++ (B.UTF8Impl.concat_sep_map comma B.Descriptor.external_utf8_of_java_type (params :> B.Descriptor.java_type list))
    ++ closing_parenthesis

let instrument_code method_name param_types is_static code =
(*
  (bc_print (method_name ^ " : ")) @
  (bc_print_utf8 (utf8_of_method_desc method_name param_types)) @
*)
  (bc_send_event method_name param_types is_static) @
  code

let has_static_flag flags =
  let is_static_flag = function
    | `Static -> true
    | _ -> false in
  List.exists is_static_flag flags

let instrument_method = function
  | B.Method.Regular r -> (
      let param_types = r.B.Method.descriptor in
      let is_static = has_static_flag r.B.Method.flags in
      let inst_code = instrument_code r.B.Method.name param_types is_static in
      let fold attrs = function
	| `Code code ->
	    let new_instructions = inst_code code.B.Attribute.code in
	    (* TODO: proper calculation of stack size *)
	    let ensure_three u = if u = B.Utils.u2 0 or u = B.Utils.u2 1 or u = B.Utils.u2 2 then B.Utils.u2 3 else u in
	    let ensure_four u = let uu = ensure_three u in if uu = B.Utils.u2 3 then B.Utils.u2 4 else uu in
	    let new_max_stack = ensure_four code.B.Attribute.max_stack in
	    let new_max_locals = ensure_three code.B.Attribute.max_locals in
	    let instrumented_code =
	      {code with
		 B.Attribute.code = new_instructions;
		 B.Attribute.max_stack = new_max_stack;
		 B.Attribute.max_locals = new_max_locals
	      } in
	      (`Code instrumented_code) :: attrs
	| a -> a :: attrs in
      let instrumented_attributes = List.rev (List.fold_left fold [] r.B.Method.attributes) in
	B.Method.Regular {r with B.Method.attributes = instrumented_attributes} )
  | m -> m

let instrument_class (c, fn) =
  let instrumented_methods = List.map instrument_method c.B.ClassDefinition.methods in
    [({c with B.ClassDefinition.methods = instrumented_methods}, fn)]

let output_class (c, fn) =
  let bytes = B.ClassDefinition.encode c in
    B.ClassFile.write bytes (B.OutputStream.make_of_channel (open_out fn))

let output_classes = List.iter output_class

let read_properties fs =
  let e p = List.map (fun x -> x.A.ast) p.A.program_properties in
  fs >> List.map Helper.parse >>= e

let process_properties ps =
  let vs p =
      let vs = get_vertices p in
      List.map (fun v -> (p, v)) vs in
  to_ints vertex (ps >>= vs);
  let named s p = Hashtbl.find vertex (p, s) in
  let starts = List.map (named "start") ps in
  let errors = List.map (named "error") ps in
  todo ()

let method_patterns ps = todo ()

let instrument_bytecode _ _ = todo ()
let generate_checkers _ _ = todo ()

let () =
  let fs = ref [] in
  let cp = ref (try Sys.getenv "CLASSPATH" with Not_found -> ".") in
  Arg.parse ["-cp", Arg.Set_string cp, "classpath"] (fun x -> fs := x :: !fs)
    "usage: ./instrumenter [-cp <classpath>] <property_files>";
  let ps = read_properties !fs in
  let ps = process_properties ps in
  let ms = method_patterns ps in
  let ms = instrument_bytecode !cp ms in
  generate_checkers ps ms

(*
let () = ()
  >> classpath
  >>= classfiles_of_path
  >>= classes_of_classfile
  >>= instrument_class
  >> output_classes

let () =
  printf "@[";
  printf "package topl;@\n";
  printf "@[<2>public class CheckerTests {@\n";
  for i = 1 to Array.length Sys.argv - 1 do
    file Sys.argv.(i)
  done;
  printf "@[<2>public static void main(String[] args) {@\n";
  printf "System.out.println(\"TODO\");@]@\n}";
  printf "@]@\n}@."
*)

(*
vim:tw=0:
*)


