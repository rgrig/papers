(* modules *) (* {{{ *)
module A = Ast
module B = BaristaLibrary
module PA = Ast.PropertyAst
open Format
open Util
(* }}} *)
(* used to communicate between conversion and instrumentation *) (* {{{ *)
type method_ =
  { name : string
  ; arity : int }

type pattern =
  { pattern_re : Str.regexp
  ; pattern_arity: int }

let patterns : (pattern, int list) Hashtbl.t = Hashtbl.create 13
  (* maps patterns to sets of event ids *)

(* }}} *)
(* representation of automata in Java *) (* {{{ *)

(*
  The instrumenter has three phases:
    - convert the automaton to an intermediate representation
    - instrument the bytecode
    - print the Java representation of the automaton
  A pattern like "c.m()" in the property matches method m in all classes that
  extend c (including c itself). For efficiency, the Java automaton does not
  know anything about inheritance. While the bytecode is instrumented all the
  methods m in classes extending c get unique identifiers and the pattern
  "c.m()" is mapped to the set of those identifiers.

  The (first) conversion
    - goes from edge list to adjacency list
    - glues all input properties into one
    - changes the vertex representation from strings to integers
    - changes automaton variable representation from strings to integers
    - normalizes method patterns (by processing "using prefix", ... )
    - collects all patterns
  During printing a bit more processing is needed to go to the Java
  representation, but only very simple stuff.
 *)

type tag = int
type vertex = int
type variable = int
type value = string (* Java literal *)

type atomic_condition =
  | AC_var of variable * int
  | AC_ct of value * int

type literal_condition =
  | LC_pos of atomic_condition
  | LC_neg of atomic_condition

type guard =
  { pattern : pattern
  ; condition : literal_condition list }

type action = (variable * int) list

type step =
  { guard : guard
  ; action : action }

type transition =
  { steps : step list
  ; target : vertex }

type vertex_data =
  { vertex_property : PA.t
  ; vertex_name : PA.vertex
  ; outgoing_transitions : transition list }

type automaton =
  { vertices : vertex_data array
  ; pattern_tags : (pattern, tag list) Hashtbl.t }
  (* The keys of {pattern_tags} are filled in during the initial conversion,
    but the values (the tag list) is filled in while the code is being
    instrumented. *)

(* }}} *)
(* small functions that help handling automata *) (* {{{ *)
let to_ints h xs = (* TODO: get rid of globals, make this create hashes *)
  let c = ref (-1) in
  Hashtbl.clear h;
  let f x = if not (Hashtbl.mem h x) then (incr c; Hashtbl.add h x !c) in
  List.iter f xs

let get_properties x =
  x.vertices >> Array.map (function {vertex_property=p;_} -> p) >> Array.to_list

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

(* }}} *)
(* pretty printing to Java *) (* {{{ *)

let array_foldi f z xs =
  let r = ref z in
  for i = 0 to Array.length xs - 1 do r := f !r i xs.(i) done;
  !r

let starts x =
  let f ks k = function
    | {vertex_name="start";_} -> k :: ks
    | _ -> ks in
  array_foldi f [] x.vertices

let escape_java_string s = s (* TODO *)

let errors x =
  let f = function
    | {vertex_name="error"; vertex_property={PA.message=e;_};_} ->
        "\"" ^ escape_java_string e ^ "\""
    | _ -> "null" in
  x.vertices >> Array.map f >> Array.to_list

let compute_interesting_events x =
  let iop = Hashtbl.create 13 in
  to_ints iop (get_properties x);
  let ieop = Array.make (Hashtbl.length iop) IntSet.empty in
  let fs acc s = add_ints acc (Hashtbl.find x.pattern_tags s.guard.pattern) in
  let ft acc t = List.fold_left fs acc t.steps in
  let fv acc v = List.fold_left ft acc v.outgoing_transitions in
  let iv v =
    let i = Hashtbl.find iop v.vertex_property in
    ieop.(i) <- fv ieop.(i) v in
  Array.iter iv x.vertices;
  let lam f x = List.map f (Array.to_list x) in
  (lam (fun v -> Hashtbl.find iop v.vertex_property) x.vertices,
  lam IntSet.elements ieop)

let rec pp_list pe ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "@\n%a" pe x
  | x :: xs -> fprintf ppf "@\n%a,%a" pe x (pp_list pe) xs

let pp_int f x = fprintf f "%d" x
let pp_string f x = fprintf f "%s" x

let pp_int_list f xs =
  fprintf f "@[<2>new int[]{%a}@]" (pp_list pp_int) xs

let pp_atomic_condition f = function
  | AC_var (x, i) -> fprintf f "new StoreEqualityGuard(%d, %d)" i x
  | AC_ct (v, i) -> fprintf f "new ConstantEqualityGuard(%d, %s)" i v

let pp_pattern tags f p =
  pp_int_list f (Hashtbl.find tags p)

let pp_literal_condition f = function
  | LC_pos c -> fprintf f "%a" pp_atomic_condition c
  | LC_neg c -> fprintf f "new NotGuard(%a)" pp_atomic_condition c

let pp_assignment f (x, i) =
  fprintf f "new Action.Assignment(%d, %d)" x i

let pp_condition f a =
  fprintf f "@[<2>new AndGuard(new Guard[]{%a})@]" (pp_list pp_literal_condition) a

let pp_guard tags f {pattern=p; condition=cs} =
  fprintf f "@[<2>%a@],@\n@[<2>%a)@]" (pp_pattern tags) p pp_condition cs

let pp_action f a =
  fprintf f "@[<2>new Action(new Assignment[]{%a})@]" (pp_list pp_assignment) a

let pp_step tags f {guard=g; action=a} =
  fprintf f "@[<2>new TransitionStep(%a, %a)@]" (pp_guard tags) g pp_action a

let pp_transition tags f {steps=ss;target=t} =
  fprintf f "@[<2>new Transition(@[<2>new TransitionStep[]{%a}@],@\n%d)@]" (pp_list (pp_step tags)) ss t

let pp_vertex tags f {outgoing_transitions=ts;_} =
  fprintf f "@[<2>new Transition[]{%a}@]" (pp_list (pp_transition tags)) ts

let pp_automaton f x =
  let pov, ieop = compute_interesting_events x in
  fprintf f "package topl;@\n@\n";
  fprintf f "import static topl.Checker.*;@\n@\n";
  fprintf f "@[<2>public class Property {@\n";
  fprintf f   "@[<2>public static Checker checker = new Checker(new Automaton(@\n";
  fprintf f     "%a,@\n" pp_int_list (starts x);
  fprintf f     "@[<2>new String[]{%a}@],@\n" (pp_list pp_string) (errors x);
  fprintf f     "@[<2>new Transition[][]{%a}@],@\n" (pp_list (pp_vertex x.pattern_tags)) (Array.to_list x.vertices);
  fprintf f     "%a,@\n" pp_int_list pov;
  fprintf f     "@[<2>new int[][]{%a}@]" (pp_list pp_int_list) ieop;
  fprintf f   "@]));@\n";
  fprintf f "@]@\n}@\n"

(* }}} *)
(* conversion to Java representation *) (* {{{ *)

(* globals used by conversion to Java representation *)
let vertex : ((PA.t * PA.vertex), vertex) Hashtbl.t = Hashtbl.create 13
let variable : (A.variable, variable) Hashtbl.t = Hashtbl.create 13

let transform_guard g = todo ()
(*
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
*)

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

let transform_action _ = todo ()

let transform_label {PA.label_guard=g; PA.label_action=a} =
  { guard = transform_guard g
  ; action = transform_action a }

let transform_properties ps = todo ()
(*
  let vs p = p >> get_vertices >> List.map (fun v -> (p, v)) in
  to_ints vertex (ps >>= vs);
  let named s p = Hashtbl.find vertex (p, s) in
  let q =
    { starts = List.map (named "start") ps
    ; errors = List.map (named "error") ps
    ; adjacency = Array.make (Hashtbl.length vertex) [] } in
  let pe p {PA.edge_source=s;PA.edge_target=t;PA.edge_labels=ls} =
    let s = Hashtbl.find vertex (p, s) in
    let t = Hashtbl.find vertex (p, t) in
    let ls = List.map transform_label ls in
    q.adjacency.(s) <- {steps=ls; target=t} :: q.adjacency.(s) in
  List.iter (fun p -> List.iter (pe p) p.PA.edges) ps;
  todo ()
*)

(* }}} *)
(* bytecode instrumentation *) (* {{{ *)
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

let get_call_id method_names nr_params = Some (Hashtbl.hash (method_names, nr_params, true))
let get_return_id s n = Some (Hashtbl.hash (s, n, false))

let bc_send_event id param_types is_static =
  let fold (instructions, i) _ =
    ((bc_array_set is_static i) :: instructions, succ i) in
  let (inst_lists, _) = List.fold_left fold ([], 0) param_types in
  let instructions = List.flatten (List.rev inst_lists) in
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

let rec add_return_code return_code = function
  | [] -> []
  | (B.Instruction.ARETURN :: instructions)
  | (B.Instruction.DRETURN :: instructions)
  | (B.Instruction.FRETURN :: instructions)
  | (B.Instruction.IRETURN :: instructions)
  | (B.Instruction.LRETURN :: instructions)
  | (B.Instruction.RETURN :: instructions)  (* do not instrument RET or WIDERET *)
    -> return_code @ (B.Instruction.RETURN :: (add_return_code return_code instructions))
  | (instr :: instructions) -> instr :: (add_return_code return_code instructions)

let instrument_code call_id return_id param_types is_static code =
  let bc_send_call_event = match call_id with
    | None -> []
    | Some id -> bc_send_event id param_types is_static in
  let bc_send_return_event = match return_id with
    | None -> []
    | Some id -> bc_send_event id param_types is_static in
(*
  (bc_print (method_name ^ " : ")) @
  (bc_print_utf8 (utf8_of_method_desc method_name param_types)) @
*)
  bc_send_call_event @
  (add_return_code bc_send_return_event code)

let has_static_flag flags =
  let is_static_flag = function
    | `Static -> true
    | _ -> false in
  List.exists is_static_flag flags

let rec get_ancesters h m c =
  try
    let (ms, parents) = Hashtbl.find h c in
      (* should check for number of parameters somehow *)
    let here = if List.mem m ms then [c] else [] in
    here @ (parents >>= get_ancesters h m)
  with Not_found -> []
  
let get_overrides h c (m, n) =
  let ancestors = get_ancesters h m c in
  let uts = B.Utils.UTF8.to_string in
  let cts c = uts (B.Name.external_utf8_for_class c) in
  let m = uts (B.Name.utf8_for_method m) in
  let qualify c =  (cts c) ^ "." ^ m in
  List.map qualify ancestors

let instrument_method h c = function
  | B.Method.Regular r -> (
      let param_types, _ = r.B.Method.descriptor in (* return is not used *)
      let nr_params = List.length param_types in
      let overrides = get_overrides h c (r.B.Method.name, nr_params) in
      let call_id = get_call_id overrides nr_params in
      let return_id = get_return_id overrides nr_params in
	match call_id, return_id with
	  | None, None -> B.Method.Regular r
	  | _ -> (
	      let is_static = has_static_flag r.B.Method.flags in
	      let inst_code = instrument_code call_id return_id param_types is_static in
	      let inst_attrs = function
		| `Code code ->
		    let new_instructions = inst_code code.B.Attribute.code in
		      (* TODO: proper calculation of stack size               *)
		      (*       the below is not good enough for return events *)
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
		      `Code instrumented_code
		| a -> a in
	      let instrumented_attributes = List.map inst_attrs r.B.Method.attributes in
		B.Method.Regular {r with B.Method.attributes = instrumented_attributes}
	    )
    )
  | m -> m

let instrument_class h (c, fn) =
  let instrumented_methods = List.map (instrument_method h c.B.ClassDefinition.name) c.B.ClassDefinition.methods in
    [({c with B.ClassDefinition.methods = instrumented_methods}, fn)]

let output_class (c, fn) =
  let bytes = B.ClassDefinition.encode c in
    B.ClassFile.write bytes (B.OutputStream.make_of_channel (open_out fn))

let output_classes = List.iter output_class

(* }}} *)
(* main *) (* {{{ *)

(*
type inheritance_node =
    {
      class_name : B.Name.for_class;
      class_methods : B.Name.for_method list;
      class_parents : inheritance_node list
    }
*)

let compute_inheritance classpath =
  let h = Hashtbl.create 101 in
(*
  let insert_node name method_names parent_names =
    let parent_nodes = List.map (Hashtbl.find h) parent_names in
    Hashtbl.add h name {class_name = name; class_methods = method_names; class_parents = parent_nodes} in
*)
  let record_class (c, _) =
    let name = c.B.ClassDefinition.name in
    let fold mns = function
      | B.Method.Regular r ->
	  let (ps, _) = r.B.Method.descriptor in (* return is not used *)
	  (r.B.Method.name, List.length ps) :: mns
      | _ -> mns in
    let method_names = List.fold_left fold [] c.B.ClassDefinition.methods in
    let parents = match c.B.ClassDefinition.extends with
      | None -> c.B.ClassDefinition.implements
      | Some e -> e::c.B.ClassDefinition.implements in
(*    insert_node name method_names parents *)
    Hashtbl.add h name (method_names, parents)
  in
  classpath
  >> classfiles_of_path
  >>= classes_of_classfile
  >> (List.iter record_class); h

let read_properties fs =
  let e p = List.map (fun x -> x.A.ast) p.A.program_properties in
  fs >> List.map Helper.parse >>= e

let method_patterns ps = todo ()

let instrument_bytecode _ = todo ()
let generate_checkers _ = todo ()

let () =
  let fs = ref [] in
  let cp = ref (try Sys.getenv "CLASSPATH" with Not_found -> ".") in
  Arg.parse ["-cp", Arg.Set_string cp, "classpath"] (fun x -> fs := x :: !fs)
    "usage: ./instrumenter [-cp <classpath>] <property_files>";
  let h = compute_inheritance !cp in
  let ps = read_properties !fs in
  let ps = transform_properties ps in
  instrument_bytecode !cp;
  generate_checkers ps

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

(* TODO:
  - Don't forget that methods in package "topl" should not be instrumented.
 *)


(*
vim:tw=0:
*)

(* }}} *)
