(* modules *) (* {{{ *)
module B = BaristaLibrary
module PA = PropAst
module SA = SoolAst
open Format
open Util
(* }}} *)
(* globals *) (* {{{ *)
let out_dir = ref "out"

(* }}} *)
(* used to communicate between conversion and instrumentation *) (* {{{ *)
type method_ =
  { method_name : string
  ; method_arity : int }

type re_pattern =
  { pattern_re : Str.regexp
  ; pattern_type : PA.event_type
  ; pattern_arity: int option }

type pattern = PAT_true | PAT_re of re_pattern


(* }}} *)
(* representation of automata in Java *) (* {{{ *)

(*
  The instrumenter has three phases:
    - convert the automaton to an intermediate representation
    - instrument the bytecode
    - emit the Java representation of the automaton
  A pattern like "c.m()" in the property matches method m in all classes that
  extend c (including c itself). For efficiency, the Java automaton does not
  know anything about inheritance. SA.While the bytecode is instrumented all the
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
  ; pattern_tags : (re_pattern, tag list) Hashtbl.t }
  (* The keys of {pattern_tags} are filled in during the initial conversion,
    but the values (the tag list) is filled in while the code is being
    instrumented. *)

(* }}} *)
(* small functions that help handling automata *) (* {{{ *)
let to_ints xs =
  let h = Hashtbl.create 101 in
  let c = ref (-1) in
  let f x = if not (Hashtbl.mem h x) then (incr c; Hashtbl.add h x !c) in
  List.iter f xs; h

let inverse_index f h =
  let r = Array.make (Hashtbl.length h) None in
  let one k v = assert (r.(v) = None); r.(v) <- Some (f k) in
  Hashtbl.iter one h;
  Array.map from_some r

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
  let iop = to_ints (get_properties x) in
  let ieop = Array.make (Hashtbl.length iop) IntSet.empty in
  let fs acc s = match s.guard.pattern with
    | PAT_true -> acc
    | PAT_re p ->  add_ints acc (Hashtbl.find x.pattern_tags p) in
  let ft acc t = List.fold_left fs acc t.steps in
  let fv acc v = List.fold_left ft acc v.outgoing_transitions in
  let iv v =
    let i = Hashtbl.find iop v.vertex_property in
    ieop.(i) <- fv ieop.(i) v in
  Array.iter iv x.vertices;
  (Array.map (fun v -> Hashtbl.find iop v.vertex_property) x.vertices,
   Array.map IntSet.elements ieop)

let pp_array pe ppf a =
  let l = Array.length a in
  if l > 0 then fprintf ppf "@\n%a" pe (0, a.(0));
  for i = 1 to l - 1 do fprintf ppf "@\n%a," pe (i, a.(i)) done

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

let pp_pattern tags obs f = function
  | PAT_true -> pp_int_list f obs
  | PAT_re p -> pp_int_list f (Hashtbl.find tags p)

let pp_literal_condition f = function
  | LC_pos c -> fprintf f "%a" pp_atomic_condition c
  | LC_neg c -> fprintf f "new NotGuard(%a)" pp_atomic_condition c

let pp_assignment f (x, i) =
  fprintf f "new Action.SA.Assignment(%d, %d)" x i

let pp_condition f a =
  fprintf f "@[<2>new AndGuard(new Guard[]{%a})@]" (pp_list pp_literal_condition) a

let pp_guard tags obs f {pattern=p; condition=cs} =
  fprintf f "@[<2>%a@],@\n@[<2>%a)@]" (pp_pattern tags obs) p pp_condition cs

let pp_action f a =
  fprintf f "@[<2>new Action(new SA.Assignment[]{%a})@]" (pp_list pp_assignment) a

let pp_step tags obs f {guard=g; action=a} =
  fprintf f "@[<2>new TransitionStep(%a, %a)@]" (pp_guard tags obs) g pp_action a

let pp_transition tags obs f {steps=ss;target=t} =
  fprintf f "@[<2>new Transition(@[<2>new TransitionStep[]{%a}@],@\n%d)@]" (pp_list (pp_step tags obs)) ss t

let pp_vertex tags pov ieop f (vi, {outgoing_transitions=ts;_}) =
  let obs = Array.get ieop (Array.get pov vi) in
  fprintf f "@[<2>new Transition[]{%a}@]" (pp_list (pp_transition tags obs)) ts

let pp_automaton f x =
  let pov, ieop = compute_interesting_events x in
  fprintf f "package topl;@\n@\n";
  fprintf f "import static topl.Checker.*;@\n@\n";
  fprintf f "@[<2>public class Property {@\n";
  fprintf f   "@[<2>public static Checker checker = new Checker(new Automaton(@\n";
  fprintf f     "%a,@\n" pp_int_list (starts x);
  fprintf f     "@[<2>new String[]{%a}@],@\n" (pp_list pp_string) (errors x);
  fprintf f     "@[<2>new Transition[][]{%a}@],@\n" (pp_array (pp_vertex x.pattern_tags pov ieop)) x.vertices;
  fprintf f     "%a,@\n" pp_int_list (Array.to_list pov);
  fprintf f     "@[<2>new int[][]{%a}@]" (pp_list pp_int_list) (Array.to_list ieop);
  fprintf f   "@]));@\n";
  fprintf f "@]@\n}@\n"

(* }}} *)
(* conversion to Java representation *) (* {{{ *)

(*
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
  fprintf ppf "new Checker.Action.SA.Assignment(%d, %d)"
    (Hashtbl.find variable av) ev

let action ppf xs =
  fprintf ppf "@[<2>new Checker.Action(new Checker.Action.SA.Assignment[]{%a})@]" (pp_list assignment) xs

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
*)

let mk_pattern t gs = todo ()

let transform_guard g =
  match PA.dnf g with
    | [g] ->
      let split (t, gs) = function
        | PA.Not (PA.Atomic (PA.Event _)) -> failwith "Not from TOPL"
        | PA.Atomic (PA.Event t') -> assert (t = None); Some t', gs
        | g -> t, g :: gs in
      let t, gs = List.fold_left split (None, []) g in
      mk_pattern t gs
    | _ -> failwith "Not from TOPL"

let transform_action _ = todo ()

let transform_label {PA.label_guard=g; PA.label_action=a} =
  { guard = transform_guard g
  ; action = transform_action a }

let transform_properties ps =
  let vs p = p >> get_vertices >> List.map (fun v -> (p, v)) in
  let iov = to_ints (ps >>= vs) in
  let mk_vd (p, v) =
    { vertex_property = p
    ; vertex_name = v
    ; outgoing_transitions = [] } in
  let p =
    { vertices = inverse_index mk_vd iov
    ; pattern_tags = Hashtbl.create 13 } in
  let add_transition vi t =
    let ts = p.vertices.(vi).outgoing_transitions in
    p.vertices.(vi) <- {p.vertices.(vi) with outgoing_transitions = t :: ts} in
  let pe p {PA.edge_source=s;PA.edge_target=t;PA.edge_labels=ls} =
    let s = Hashtbl.find iov (p, s) in
    let t = Hashtbl.find iov (p, t) in
    let ls = List.map transform_label ls in
    add_transition s {steps=ls; target=t} in
  List.iter (fun p -> List.iter (pe p) p.PA.edges) ps;
  p

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


let string_of_method_name mn =
  B.Utils.UTF8.to_string (B.Name.utf8_for_method mn)

let mk_method mn ma =
  { method_name = string_of_method_name mn
  ; method_arity = ma }

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

let does_method_match
  ({ method_name=mn; method_arity=ma }, mt)
  { pattern_re=re; pattern_type=t; pattern_arity=a }
=
  option true ((=) ma) a &&
  mt = t &&
  Str.string_match re mn 0

let get_tag x =
  let cnt = ref (-1) in fun t (mns, ma) ->
  let fp p _ acc =
    let cm mn = does_method_match ({method_name=mn; method_arity=ma}, t) p in
    if List.exists cm mns then p :: acc else acc in
  match Hashtbl.fold fp x.pattern_tags [] with
    | [] -> None
    | ps ->
        incr cnt;
        let at p =
          let ts = Hashtbl.find x.pattern_tags p in
          Hashtbl.replace x.pattern_tags p (!cnt :: ts) in
        List.iter at ps;
        Some !cnt

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

let rec get_ancestors h m c =
  try
    let (ms, parents) = Hashtbl.find h c in
    let here = if List.mem m ms then [c] else [] in
    here @ (parents >>= get_ancestors h m)
  with Not_found -> []

let get_overrides h c ({method_name=n; method_arity=a} as m) =
  let ancestors = get_ancestors h m c in
  let uts = B.Utils.UTF8.to_string in
  let cts c = uts (B.Name.external_utf8_for_class c) in
  let qualify c =  (cts c) ^ "." ^ n in
  (List.map qualify ancestors, a)

let instrument_method get_tag h c = function
  | B.Method.Regular r -> (
      let param_types, _ = r.B.Method.descriptor in (* return is not used *)
      let nr_params = List.length param_types in
      let overrides =
        get_overrides h c (mk_method r.B.Method.name nr_params) in
      let call_id = get_tag PA.Call overrides in
      let return_id = get_tag PA.Return overrides in
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

let open_class_channel c =
  let fn = B.Name.internal_utf8_for_class c.B.ClassDefinition.name in
  let fn = B.Utils.UTF8.to_string fn in
  let fn = Filename.concat !out_dir fn in
  mkdir_p (Filename.dirname fn);
  open_out fn

let output_class c =
  let ch = open_class_channel c in
  let bytes = B.ClassDefinition.encode c in
  B.ClassFile.write bytes (B.OutputStream.make_of_channel ch);
  close_out ch

let instrument_class get_tags h (c, fn) =
  let instrumented_methods = List.map (instrument_method get_tags h c.B.ClassDefinition.name) c.B.ClassDefinition.methods in
  output_class {c with B.ClassDefinition.methods = instrumented_methods}

let iter_classes cp f = (* TODO *)
  cp
  >> classfiles_of_path
  >>= classes_of_classfile
  >> List.iter f

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
          mk_method r.B.Method.name (List.length ps) :: mns
      | _ -> mns in
    let method_names = List.fold_left fold [] c.B.ClassDefinition.methods in
    let parents = match c.B.ClassDefinition.extends with
      | None -> c.B.ClassDefinition.implements
      | Some e -> e::c.B.ClassDefinition.implements in
(*    insert_node name method_names parents *)
    Hashtbl.add h name (method_names, parents)
  in
  iter_classes classpath record_class;
  h

let instrument_bytecode get_tag cp h =
  iter_classes cp (instrument_class get_tag h)

(* }}} *)
(* main *) (* {{{ *)

let read_properties fs =
  let e p = List.map (fun x -> x.PA.ast) p.SA.program_properties in
  fs >> List.map Helper.parse >>= e

let generate_checkers _ = todo ()

let () =
  let fs = ref [] in
  let cp = ref (try Sys.getenv "CLASSPATH" with Not_found -> ".") in
  Arg.parse ["-cp", Arg.Set_string cp, "classpath"] (fun x -> fs := x :: !fs)
    "usage: ./instrumenter [-cp <classpath>] <property_files>";
  let h = compute_inheritance !cp in
  let ps = read_properties !fs in
  let p = transform_properties ps in
  instrument_bytecode (get_tag p) !cp h;
  generate_checkers p

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
