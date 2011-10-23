open Format
open Util

module B = BaristaLibrary
module BA = B.Attribute
module BM = B.Method
module BCd = B.ClassDefinition

(* {{{ iterate class path *)
let endswith suffix s =
  let m = String.length suffix in
  let n = String.length s in
  if m > n then false else String.sub s (n - m) m = suffix

let contains_no undesired s =
  let r = Str.regexp_string undesired in
    try let _ = Str.search_forward r s 0 in false
    with Not_found -> true

let classfiles_of_path =
  fs_filter (fun p -> contains_no "/topl/" p && endswith ".class" p)

(* Opens the file, last return argument is a channel to be closed *)
let classes_of_classfile fn =
  try
    let ch = open_in fn in
       ch
    >> B.InputStream.make_of_channel
    >> B.ClassFile.read
    >> B.ClassDefinition.decode
    >> (fun x -> [(x, fn, ch)])
  with
  | B.InputStream.Exception e ->
      eprintf "@[%s: %s@." fn (B.InputStream.string_of_error e); []
  | _ ->
      eprintf "@[%s: error@." fn; []

let iter_classes cp f =
  let process_classfile cf = List.iter f (classes_of_classfile cf) in
  List.iter process_classfile (classfiles_of_path cp)

let iter_and_close_classes cp f =
  let process (x, fn, ch) = f (x, fn); close_in ch in
  iter_classes cp process
(* }}} *)

let overwrite = ref false

let input_class fn =
  printf "@[Decoding %s@." fn;
  let ch = open_in fn in
    ch
    >> B.InputStream.make_of_channel
    >> B.ClassFile.read
    >> B.ClassDefinition.decode
    >> (fun x -> close_in ch; x)

let open_class_channel c =
  let fn = B.Name.internal_utf8_for_class c.B.ClassDefinition.name in
  let fn = B.Utils.UTF8.to_string fn in
  let fn = if !overwrite then fn ^ ".class" else fn ^ ".class.touched" in
(*  let fn = Filename.concat !out_dir fn in *)
  mkdir_p (Filename.dirname fn);
  open_out fn

let removeLNT c =
  let not_LNT : BA.code_attribute -> bool = function
    | `LineNumberTable _ -> false
    | _ -> true in
  let rm_c c = { c with BA.attributes = List.filter not_LNT c.BA.attributes } in
  let rm_a : BA.for_method -> BA.for_method = function
    | `Code c -> `Code (rm_c c)
    | x -> x in
  let rm_mr mr = { mr with BM.attributes = List.map rm_a mr.BM.attributes } in
  let rm_mc mc = { mc with BM.cstr_attributes = List.map rm_a mc.BM.cstr_attributes } in
  let rm_mi mi = { mi with BM.init_attributes = List.map rm_a mi.BM.init_attributes } in
  let rm_m = function
    | BM.Regular mr -> BM.Regular (rm_mr mr)
    | BM.Constructor mc -> BM.Constructor (rm_mc mc)
    | BM.Initializer mi -> BM.Initializer (rm_mi mi) in
  { c with
    BCd.methods = List.map rm_m c.BCd.methods }

let output_class c =
  let ch = open_class_channel c in
    printf "@[...  encoding@.";
    try
      let bytes = B.ClassDefinition.encode c in
	printf "@[...  writing file@.";
	B.ClassFile.write bytes (B.OutputStream.make_of_channel ch);
	close_out ch
    with (B.Instruction.Exception err) ->
	close_out ch;
      printf "@[%s@\n@]" (B.Instruction.string_of_error err)

let process_class (c, _) = output_class (removeLNT c)

let () =
  let cp = ref "." in
  Arg.parse ["-overwrite", Arg.Set overwrite, "overwrite"] (fun x -> cp := x)
    "usage: /id_instr [-overwrite] <class path>";
  iter_and_close_classes !cp process_class
