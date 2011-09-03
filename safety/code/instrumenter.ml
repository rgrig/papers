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

let bc_print s = [
  Instruction.GETSTATIC (java_lang_System, out, `Class java_io_PrintStream); 
  Instruction.LDC (`String (utf8 s)); 
  Instruction.INVOKEVIRTUAL (`Class_or_interface java_io_PrintStream, 
			     println, 
			     ([`Class java_lang_String], `Void)); 
]

let instrument_code code method_name = (bc_print (method_name ^ " got called")) @ code

let instrument_method = function
  | Method.Regular r -> (
      let fold attrs = function
	| (`Code code) ->
	    let method_name = r.Method.name >> Name.utf8_for_method >> Utils.UTF8.to_string in
	    let new_instructions = instrument_code code.Attribute.code method_name in
	    let instrumented_code = {code with Attribute.code = new_instructions} in
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
