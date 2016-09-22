open Expr

let rec generate_expr (n : int) =
  if (n=0) then string_of_int(Random.int 10)
  else
    let r = Random.int 5 in
    match r with
    | 0 -> string_of_int(Random.int 10)
    | 1 -> generate_expr(n-1) ^ "+" ^ generate_expr(n-1)
    | 2 -> generate_expr(n-1) ^ "+" ^ generate_expr(n-1) (* only a + for now, - has issues *)
    | 3 -> generate_expr(n-1) ^ "*" ^ generate_expr(n-1)
    | 4 -> "(" ^ generate_expr(n-1) ^ ")"
    | _ -> generate_expr(n-1)



let _ =
  Random.self_init();
  print_string(generate_expr(12))
