(* hw 2-2 of 2017-16140 *)
exception FreeVariable
type exp = X | INT of int | REAL of float
          | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp
          | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp
let rec calculate: exp -> float = fun e ->
  (* evalutate funcion with variable X for sigma, integral *)
  let rec eval: exp -> float -> float = fun exp a ->
    match exp with
    | X -> a
    | INT x -> float_of_int x
    | REAL x -> x
    | ADD (e1, e2) -> (eval e1 a) +. (eval e2 a)
    | SUB (e1, e2) -> (eval e1 a) -. (eval e2 a)
    | MUL (e1, e2) -> (eval e1 a) *. (eval e2 a)
    | DIV (e1, e2) -> (eval e1 a) /. (eval e2 a)
    | SIGMA (e1, e2, e3) ->
        let p = int_of_float (eval e1 a) in
        let q = int_of_float (eval e2 a) in
        if p > q then 0. else (eval e3 (float_of_int p)) +. (eval (SIGMA(INT(p+1),e2, e3)) a)
    | INTEGRAL (e1, e2, e3) ->
        let p = eval e1 a in
        let q = eval e2 a in
        let diff = p -. q in
        if diff < 0.1 && diff > -0.1 then 0.
        else if p < q then ((eval e3 p)*.0.1) +. (eval (INTEGRAL((REAL(p+.0.1)), e2, e3)) a)
        else -.(eval (INTEGRAL(e2, e1, e3)) a)
  in
  (* calculate *)
  match e with
  | X -> raise FreeVariable
  | INT a -> float_of_int a
  | REAL a -> a
  | ADD (e1, e2) -> (calculate e1) +. (calculate e2)
  | SUB (e1, e2) -> (calculate e1) -. (calculate e2)
  | MUL (e1, e2) -> (calculate e1) *. (calculate e2)
  | DIV (e1, e2) -> (calculate e1) /. (calculate e2)
  | SIGMA (e1, e2, e3) ->
      let a = int_of_float (calculate e1) in
      let b = int_of_float (calculate e2) in
      if a > b then 0. else (eval e3 (float_of_int a)) +. (calculate (SIGMA ((INT(a+1)), e2, e3)))
  | INTEGRAL (e1, e2, e3) ->
      let a = calculate e1 in
      let b = calculate e2 in
      let diff = a -. b in
      if diff < 0.1 && diff > -0.1 then 0.
      else if a < b then ((eval e3 a)*.0.1) +. (calculate (INTEGRAL (REAL(a+.0.1), e2, e3)))
      else -.(calculate (INTEGRAL(e2, e1, e3)))