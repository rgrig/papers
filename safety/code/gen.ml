open Printf

let _ =
  Random.init (int_of_string Sys.argv.(1));
  while true do
    printf "%d\n" (Random.int 64)
  done
