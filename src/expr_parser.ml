(*
  The expression parser from the Web page, adapted to respect
  left-associativity of operators.
*)

(* function to convert digit chars into integers *)
let int_of_char c = Char.code c - Char.code '0'

(* First, we define the tokens (lexical units) *)

type token = Int of int | Plus | Minus | Times | LeftBracket | RightBracket | Divide | Power

(* Then we define the lexer, using stream parsers with only right recursion *)

let rec lex = parser (* char stream -> token stream *)
  | [< 'c when c = ' ' || c = '\t'; toks = lex >] -> toks (* spaces are ignored *)
  | [< tok = token; toks = lex >] -> [< 'tok; toks >]
    (* recognizing one token at a time *)
  | [< >] -> [< >] (* end of stream *)
and token = parser
  | [< ' ('+') >] -> Plus
  | [< ' ('-') >] -> Minus
  | [< ' ('*') >] -> Times
  | [< ' ('(') >] -> LeftBracket
  | [< ' (')') >] -> RightBracket
  | [< ' ('/') >] -> Divide
  | [< ' ('^') >] -> Power
  | [< ' ('0') >] -> Int 0
    (* special case for 0 *)
  | [< 'c when c >= '1' && c <= '9'; n = token_number (int_of_char c) >] -> Int n
    (* numbers start with 1..9 *)
and token_number num = parser (* number: 'num' is what has been recognized so far *)
  | [< 'c when c >= '0' && c <= '9'; n = token_number (num*10 + int_of_char c) >] -> n (* reading more digits *)
  | [< >] -> num (* end of number *)


(* Next, we define a type to represent abstract syntax trees: *)

type expr = Num of int | Add of expr * expr | Sub of expr * expr | Mul of expr * expr | Div of expr * expr | Pow of expr * expr

(* The recursive descent parser consists of three mutually-recursive functions: *)

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

let rec eval_expr (e : expr) =
  match e with
  | Num(n) -> n
  | Add(n,m) -> (eval_expr n) + (eval_expr m)
  | Sub(n,m) -> (eval_expr n) - (eval_expr m)
  | Mul(n,m) -> (eval_expr n) * (eval_expr m)
  | Div(n,m) -> (eval_expr n) / (eval_expr m)
  | Pow(n,m) -> int_of_float(float_of_int(eval_expr n) ** float_of_int(eval_expr m))
                              
(* The recursive descent parser consists of three mutually-recursive functions: *)

let rec parse_expr = parser
  | [< e1 = parse_factor; e2 = parse_expr_aux e1 >] -> e2

and parse_expr_aux e1 = parser
  | [< 'Plus; e2 = parse_factor; e3 = parse_expr_aux (Add (e1,e2)) >] -> e3
  | [< 'Minus; e2 = parse_factor; e3 = parse_expr_aux (Sub (e1,e2))>] -> e3
  | [< >] -> e1

and parse_factor = parser
  | [< e1 = parse_power; e2 = parse_factor_aux e1 >] -> e2

and parse_factor_aux e1 = parser
  | [< 'Times; e2 = parse_power; e3 = parse_factor_aux (Mul (e1,e2)) >] -> e3
  | [< 'Divide; e2 = parse_power; e3 = parse_factor_aux (Div (e1,e2)) >] -> e3                          
  | [< >] -> e1

and parse_power = parser
  | [< e1 = parse_atom; e2 = parse_power_aux e1 >] -> e2                   
               
and parse_power_aux e1 = parser
  | [< 'Power; e2 = parse_power >] -> Pow (e1,e2)
  | [< >] -> e1                                                                                       

(*
and parse_power = parser
    | [< e1 = parse_atom; 'Kwd "^" ?? "pow expected"; e2 = parse_power >] -> Pow (e1,e2)
    | [< e1 = parse_atom >] -> e1

This doesn't work because we cannot start two patterns with the same element 

*)
and parse_atom = parser
  | [< 'Int n >] -> Num n
  | [< 'LeftBracket; e = parse_expr; 'RightBracket >] -> e

(* That is all that is required to parse simple arithmetic
expressions. We can test it by lexing and parsing a string to get the
abstract syntax tree representing the expression: *)

let test s = parse_expr (lex (Stream.of_string s))

let _ = 
  let test_file = open_in "test" in
  let s = input_line test_file in 
  close_in test_file;
  (*  print_string s; 
  print_newline(); *)
  print_int(eval_expr (parse_expr (lex (Stream.of_string s))))




