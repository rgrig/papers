open Format

let log_cp = 1 lsl 1 (* classpath; bytecode lookup *)

let log_active = log_cp

let log x = log_active land x <> 0

let logf = std_formatter
