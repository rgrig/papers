open Format
open Util

module B = BaristaLibrary

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
    output_class c
