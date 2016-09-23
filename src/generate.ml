open Expr_parser

let rec generate_expr (n : int) =
  if (n=0) then string_of_int(Random.int 10)
  else
    let r = Random.int 7 in
    match r with
    | 0 -> string_of_int(Random.int 10)
    | 1 -> generate_expr(n-1) ^ "+" ^ generate_expr(n-1)
    | 2 -> generate_expr(n-1) ^ "-" ^ generate_expr(n-1)
    | 3 -> generate_expr(n-1) ^ "*" ^ generate_expr(n-1)
    | 4 -> "(" ^ generate_expr(n-1) ^ ")"
    | 5 -> " " ^ generate_expr(n)
    | 6 -> generate_expr(n-1) ^ "^" ^ generate_expr(n-1)
    | _ -> generate_expr(n-1)



let _ =
  Random.self_init();
  print_string(generate_expr(5))
