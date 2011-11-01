(** Maps a function over class files, taking care of (nested) jars *)
(* Contrary to normal maps, the return type is unit, *)
(* but it takes a source and target directory as input. *)
val map : string -> string -> (BaristaLibrary.ClassDefinition.t ->
  BaristaLibrary.ClassDefinition.t) -> unit
