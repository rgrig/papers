open BaristaLibrary
open Format
open Util

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

let classes_of_lowlevel_classes _ = failwith "todo"

let classes_of_classfile fn =
  try fn 
    >> open_in
    >> InputStream.make_of_channel
    >> ClassFile.read
    >> ClassDefinition.decode
    >> (fun x -> [(x, fn)])
  with
  | InputStream.Exception e ->
      eprintf "@[%s: %s@." fn (InputStream.string_of_error e); []
  | _ ->
      eprintf "@[%s: error@." fn; []

(*
let name_of_method = function
  | Method.Regular r ->
      [r.Method.name >> Name.utf8_for_method >> Utils.UTF8.to_string]
  | _ -> []
*)

let utf8 = Utils.UTF8.of_string 
let utf8_for_class x = Name.make_for_class_from_external (utf8 x) 
let utf8_for_field x = Name.make_for_field (utf8 x) 
let utf8_for_method x = Name.make_for_method (utf8 x) 
let java_lang_System = utf8_for_class "java.lang.System" 
let java_lang_String = utf8_for_class "java.lang.String" 
let java_io_PrintStream = utf8_for_class "java.io.PrintStream" 
let out = utf8_for_field "out" 
let println = utf8_for_method "println" 

let bc_print_utf8 us = [
  Instruction.GETSTATIC (java_lang_System, out, `Class java_io_PrintStream); 
  Instruction.LDC (`String us); 
  Instruction.INVOKEVIRTUAL (`Class_or_interface java_io_PrintStream, 
			     println, 
			     ([`Class java_lang_String], `Void)); 
]
let bc_print s = bc_print_utf8 (utf8 s)
let bc_print_par p = bc_print_utf8 (p.Signature.identifier)

(* Taken from disassembler.ml *)
let (++) = UTF8Impl.(++)
let space = UTF8Impl.of_string " "
let comma = UTF8Impl.of_string ","
let opening_parenthesis = UTF8Impl.of_string "("
let closing_parenthesis = UTF8Impl.of_string ")"
let utf8_of_method_desc name desc =
  let params, return = desc in
  (Descriptor.external_utf8_of_java_type return)
    ++ space
    ++ (Name.utf8_for_method name)
    ++ opening_parenthesis
    ++ (UTF8Impl.concat_sep_map comma Descriptor.external_utf8_of_java_type (params :> Descriptor.java_type list))
    ++ closing_parenthesis

let instrument_code method_name param_types code =
(*
  (bc_print (method_name ^ " : ")) @
*)
  (bc_print_utf8 (utf8_of_method_desc method_name param_types)) @
  code

let instrument_method = function
  | Method.Regular r -> (
      let param_types = r.Method.descriptor in
      let inst_code = instrument_code r.Method.name param_types in
      let fold attrs = function
	| `Code code ->
	    let new_instructions = inst_code code.Attribute.code in
	    (* TODO: proper calculation of stack size *)
	    let ensure_three u = if u = Utils.u2 0 or u = Utils.u2 1 or u = Utils.u2 2 then Utils.u2 3 else u in
	    let new_max_stack = ensure_three code.Attribute.max_stack in
	    let instrumented_code = 
	      {code with
		 Attribute.code = new_instructions;
		 Attribute.max_stack = new_max_stack
	      } in
	      (`Code instrumented_code) :: attrs
	| a -> a :: attrs in
      let instrumented_attributes = List.rev (List.fold_left fold [] r.Method.attributes) in
	Method.Regular {r with Method.attributes = instrumented_attributes} )
  | m -> m

let instrument_class (c, fn) =
  let instrumented_methods = List.map instrument_method c.ClassDefinition.methods in
    [({c with ClassDefinition.methods = instrumented_methods}, fn)]

let output_class (c, fn) =
  let bytes = ClassDefinition.encode c in 
    ClassFile.write bytes (OutputStream.make_of_channel (open_out fn))

let output_classes = List.iter output_class

let () = ()
  >> classpath
  >>= classfiles_of_path
  >>= classes_of_classfile
  >>= instrument_class
  >> output_classes
