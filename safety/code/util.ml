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

(** Function composition. *)
let (@@) f g x = f (g x)

module IntMap = Map.Make (struct type t = int let compare = compare end)
module StringMap = Map.Make (String)
module StringPairMap = Map.Make (struct type t = string * string let compare = compare end)
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

let fresh_id =
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  let n = String.length alphabet in
  let count = ref (-1) in
  fun () ->
    incr count;
    let r = Buffer.create 5 in
    let rec f x =
      if x > 0 then f (x / n);
      Buffer.add_char r alphabet.[x mod n] in
    f !count;
    if Buffer.length r = 0 then Buffer.add_char r 'a';
    Buffer.contents r

let fresh_internal_id () =
  Printf.sprintf "<%s>" (fresh_id ())
