type type_ =
    Class of string
  | Method of type_ list * type_

let type_check p = failwith "Tc.type_check not implemented"
