open Format
open Util

module B = BaristaLibrary
module BA = B.Attribute
module BM = B.Method
module BCd = B.ClassDefinition

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
  let fn = fn ^ ".class.touched" in
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

let () =
  let fn = ref "" in
  Arg.parse [] (fun x -> fn := x) "usage: /id_instr <class file>";
  let c = input_class !fn in
  output_class (removeLNT c)
