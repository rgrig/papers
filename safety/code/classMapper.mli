module B = BaristaLibrary
module BCd = B.ClassDefinition

(** Maps a function over class files, taking care of (nested) jars *)
(* Contrary to normal maps, the return type is unit, *)
(* but it takes a target directory as input. *)
val map : string -> (BCd -> BCd) -> unit