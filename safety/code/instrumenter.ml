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
    >> (fun x -> [x])
  with
  | InputStream.Exception e ->
      eprintf "@[%s: %s@." fn (InputStream.string_of_error e); []
  | _ ->
      eprintf "@[%s: error@." fn; []

let methods_of_class c = c.ClassDefinition.methods

let name_of_method = function
  | Method.Regular r ->
      [r.Method.name >> Name.utf8_for_method >> Utils.UTF8.to_string]
  | _ -> []

let () = ()
  >> classpath
  >>= classfiles_of_path
  >>= classes_of_classfile
  >>= methods_of_class
  >>= name_of_method
  >> print_list
