(* hw 1-2 of 2017-16140 *)
type expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr
type formula = TRUE
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr
let rec eval_expr: expr -> int = function
| NUM a -> a
| PLUS (e1, e2) -> eval_expr(e1) + eval_expr(e2)
| MINUS (e1, e2) -> eval_expr(e1) - eval_expr(e2)
let rec eval: formula -> bool = function
| TRUE -> true
| FALSE -> false
| NOT f -> not (eval f)
| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
| ORELSE (f1, f2) -> (eval f1) || (eval f2)
| IMPLY (f1, f2) -> if eval(f1) == false then true else if eval(f2) == true then true else false
| LESS (e1, e2) -> if eval_expr(e1) < eval_expr(e2) then true else false