let from_some = function
  | Some x -> x
  | None -> failwith "I was hoping to get Some."
