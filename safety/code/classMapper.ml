open Format

module U = Util
module B = BaristaLibrary

let (/) = Filename.concat

let is_jar f = Filename.check_suffix f ".jar"
let is_class f = Filename.check_suffix f ".class"

let mk_tmp_dir =
  let tmp_file = Filename.temp_file "instr_" "_jar" in
  Sys.remove tmp_file;
  U.mkdir_p tmp_file;
  tmp_file

let ensure_dir f =
  if Filename.check_suffix f "/" then f else (f ^ "/")

let open_class fn =
  let read_class_channel ch =
  try
    let cl_in = B.InputStream.make_of_channel ch in
    let cf = B.ClassFile.read cl_in in
    let cd = B.ClassDefinition.decode cf in
    Some cd
  with
  | B.InputStream.Exception e ->
      eprintf "@[%s: %s@." fn (B.InputStream.string_of_error e);
      None
  | _ ->
      eprintf "@[%s: error@." fn;
      None in
  try
    let ch = open_in fn in
    let cd = read_class_channel ch in
    close_in ch; cd
  with
  | _ ->
      eprintf "@[error opening %s@." fn;
      None

let output_class fn c =
  let ch = open_out fn in
  let bytes = B.ClassDefinition.encode c in
  B.ClassFile.write bytes (B.OutputStream.make_of_channel ch);
  close_out ch

let rec map in_dir out_dir f =
  let process_jar jf =
    let tmp_in_dir = mk_tmp_dir in
    let tmp_out_dir = mk_tmp_dir in
    let jar_in = Zip.open_in (in_dir / jf) in
    let extract e =
      let e_fn = tmp_in_dir / e.Zip.filename in
      U.mkdir_p (Filename.dirname e_fn);
      if not e.Zip.is_directory then Zip.copy_entry_to_file jar_in e e_fn in
    List.iter extract (Zip.entries jar_in);
    Zip.close_in jar_in;
    map tmp_in_dir tmp_out_dir f;
    U.rm_r tmp_in_dir;
    let jar_out = Zip.open_out (out_dir / jf) in
    let intract _ f =
      if Sys.is_directory f then Zip.add_entry "" jar_out (ensure_dir f)
      else Zip.copy_file_to_entry (tmp_out_dir / f) jar_out f in
    U.rel_fs_preorder tmp_out_dir intract Filename.current_dir_name;
    Zip.close_out jar_out;
    U.rm_r tmp_out_dir in
  let process_class fn =
    match open_class (in_dir / fn) with
    | None -> U.cp (in_dir / fn) (out_dir / fn)
    | Some cd ->
        let inst_cd = f cd in
        output_class (out_dir / fn) inst_cd in
  let process _ fn =
    if Sys.is_directory fn then U.mkdir_p (out_dir / fn)
    else if is_jar fn then process_jar fn
    else if is_class fn then process_class fn
    else U.cp (in_dir / fn) (out_dir / fn) in
  U.rel_fs_preorder in_dir process Filename.current_dir_name

let rec iter in_dir f =
  let iter_jar jf =
    let tmp_in_dir = mk_tmp_dir in
    let jar_in = Zip.open_in (in_dir / jf) in
    let extract e =
      let e_fn = tmp_in_dir / e.Zip.filename in
      U.mkdir_p (Filename.dirname e_fn);
      if not e.Zip.is_directory then Zip.copy_entry_to_file jar_in e e_fn in
    List.iter extract (Zip.entries jar_in);
    Zip.close_in jar_in;
    iter tmp_in_dir f;
    U.rm_r tmp_in_dir in
  let iter_class fn =
    match open_class (in_dir / fn) with
    | None -> ()
    | Some cd -> f cd in
  let process _ fn =
    if Sys.is_directory fn then ()
    else if is_jar fn then iter_jar fn
    else if is_class fn then iter_class fn in
  U.rel_fs_preorder in_dir process Filename.current_dir_name
