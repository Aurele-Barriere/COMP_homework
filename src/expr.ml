
open Genlex

let keywords = ["("; ")"; "+"; "-"; "*"; "/"; "^"]

let lex stream =
  let rec aux = parser
    | [< 'Int n when n<0; t=aux >] -> [< 'Kwd "-"; 'Int (-n); t >]
    | [< 'h; t=aux >] -> [< 'h; t >]
    | [< >] -> [< >] in
  aux (make_lexer keywords stream);;

(* Note that we are careful to replace negative integers in the input stream with a minus sign followed by a positive integer. *)

(* Next, we define a type to represent abstract syntax trees: *)

type expr = Num of int | Add of expr * expr | Sub of expr * expr | Mul of expr * expr | Div of expr * expr | Pow of expr * expr

let rec print_expr (e : expr) =
  match e with
  | Num (n) -> print_int n
  | Add(e1,e2) -> print_string("(");
                  print_expr e1;
                  print_string("+");
                  print_expr e2;
                  print_string(")")
  | Sub(e1,e2) -> print_string("(");
                  print_expr e1;
                  print_string("-");
                  print_expr e2;
                  print_string(")")
  | Mul(e1,e2) -> print_string("(");
                  print_expr e1;
                  print_string("*");
                  print_expr e2;
                  print_string(")")
  | Div(e1,e2) -> print_string("(");
                  print_expr e1;
                  print_string("/");
                  print_expr e2;
                  print_string(")")
  | Pow(e1,e2) -> print_string("(");
                  print_expr e1;
                  print_string("^");
                  print_expr e2;
                  print_string(")")
                                 
(* The recursive descent parser consists of three mutually-recursive functions: *)

let rec parse_expr = parser
  | [< e1 = parse_factor; e2 = parse_expr_aux e1 >] -> e2

and parse_expr_aux e1 = parser
  | [< 'Kwd "+"; e2 = parse_factor; e3 = parse_expr_aux (Add (e1,e2)) >] -> e3
  | [< 'Kwd "-"; e2 = parse_factor; e3 = parse_expr_aux (Sub (e1,e2)) >] -> e3
  | [< >] -> e1

and parse_factor = parser
  | [< e1 = parse_power; e2 = parse_factor_aux e1 >] -> e2

and parse_factor_aux e1 = parser
  | [< 'Kwd "*"; e2 = parse_power; e3 = parse_factor_aux (Mul (e1,e2)) >] -> e3
  | [< 'Kwd "/"; e2 = parse_power; e3 = parse_factor_aux (Div (e1,e2)) >] -> e3                          
  | [< >] -> e1

and parse_power = parser
  | [< e1 = parse_atom; e2 = parse_power_aux e1 >] -> e2                   
               
and parse_power_aux e1 = parser
  | [< 'Kwd "^"; e2 = parse_power >] -> Pow (e1,e2)
  | [< >] -> e1                                                                                       

(*
and parse_power = parser
    | [< e1 = parse_atom; 'Kwd "^" ?? "pow expected"; e2 = parse_power >] -> Pow (e1,e2)
    | [< e1 = parse_atom >] -> e1

This doesn't work because we cannot start two patterns with the same element 

*)
and parse_atom = parser
  | [< 'Int n >] -> Num n
  | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e

(* That is all that is required to parse simple arithmetic
expressions. We can test it by lexing and parsing a string to get the
abstract syntax tree representing the expression: *)

let test s = parse_expr (lex (Stream.of_string s))

let _ = print_expr (test "1^2^3^4")

(*- : expr = Sub (Add (Num 1, Mul (Num 2, Add (Num 3, Num 4))), 5) *)

             
