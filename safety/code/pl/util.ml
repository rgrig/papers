type ('a, 'b) either = Left of 'a | Right of 'b

let either a b = function Left x -> a x | Right x -> b x

let from_option a = function None -> a | Some a -> a

let from_some = function
  | Some x -> x
  | None -> failwith "I was hoping to get Some."
