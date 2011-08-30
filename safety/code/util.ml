type ('a, 'b) either = Left of 'a | Right of 'b

let either a b = function Left x -> a x | Right x -> b x

let from_option a = function None -> a | Some a -> a

let option d f = function
  | None -> d
  | Some x -> f x

let from_some = function
  | Some x -> x
  | None -> failwith "I was hoping to get Some."

(* [map_find d f p xs] applies [f] to each [x] and returns the first
  result that satisfies [p]. Otherwise returns the default [d]. *)
let rec map_find d f p = function
  | x :: xs -> let r = f x in if p r then r else map_find d f p xs
  | [] -> d
let map_find_not d f xs = map_find d f ((<>) d) xs

let map_option f xs =
  let f' acc x = match f x with
    | None -> acc
    | Some y -> y :: acc in
  List.rev (List.fold_left f' [] xs)

let cons x xs = x :: xs

let unique l =
  let h = Hashtbl.create 51 in
  List.iter (fun x -> Hashtbl.replace h x x) l;
  Hashtbl.fold (fun _ -> cons) h []

(** Function composition. *)
let (@<) f g x = f (g x)

(** Function composition (reversed). *)
let (@>) f g = g @< f

(** Function application (reversed). *)
let (>>) x f = f x

(** Map followed by concat. *)
let (>>=) x f = x >> List.map f >> List.concat

module Int = struct type t = int let compare = compare end
module OrderedPair (A:Set.OrderedType) (B:Set.OrderedType) =
struct
  type t = A.t * B.t
  let compare = compare
end

(* The type annotation on IntMap is a workaround for OCaml bug
   http://caml.inria.fr/mantis/view.php?id=5302 *)
module IntMap : (Map.S with type key = int) = Map.Make (Int)
module StringMap = Map.Make (String)
module StringPairMap = Map.Make (OrderedPair (String) (String))
module StringSet = Set.Make (String)

let flip f x y = f y x

let add_strings s = List.fold_left (flip StringSet.add) s

let pp_s pp_f s = Format.fprintf pp_f "%s" s

let pp_list pp_sep pp_element =
  let rec f = fun pp_f -> function
    | [] -> ()
    | [x] -> pp_element pp_f x
    | x :: xs -> Format.fprintf pp_f "%a%s%a" pp_element x pp_sep f xs in
  f

let pp_option pp_e ppf = function
  | None -> Format.fprintf ppf "None"
  | Some s -> Format.fprintf ppf "Some %a" pp_e s

let rec fix f x =
  let y = f x in
  if y = x then y else fix f y

let rec y f x = f (y f) x

let rec memo f f' =
  let cache = Hashtbl.create 101 in
  fun x ->
    try
      Hashtbl.find cache x
    with Not_found -> begin
      let r = f f' x in
      Hashtbl.add cache x r; r
    end

let fresh_id () =
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  let n = String.length alphabet in
  let count = ref (-1) in
  fun () ->
    incr count;
    if !count = 0 then String.sub alphabet 0 1 else begin
      let r = Buffer.create 5 in
      let rec f x =
        if x >= n then f (x / n);
        Buffer.add_char r alphabet.[x mod n] in
      f !count;
      Buffer.contents r
    end

let fresh_internal_id =
  let internal = fresh_id () in
  (fun () -> Printf.sprintf "<%s>" (internal ()))

let todo () = failwith "todo"

let list_of_option = function
  | Some x -> [x]
  | None -> []

let cartesian xss =
  let rec f acc = function
    | [] -> List.map List.rev acc
    | xs :: xss ->
        let ys = List.map (fun x -> List.map (fun ys -> x :: ys) acc) xs in
        f (List.concat ys) xss in
  f [[]] xss

let rec fs_postorder m f =
  if Sys.file_exists f then begin
    if Sys.is_directory f then begin
      let children = Array.map (Filename.concat f) (Sys.readdir f) in
      Array.iter (fs_postorder m) children
    end;
    m f
  end

let fs_filter p f =
  let r = ref [] in
  fs_postorder (fun x -> if p x then r := x::!r) f; !r
