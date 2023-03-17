(* hw 1-1 of 2017-16140 *)
let rec sigma: int * int * (int -> int) -> int
  = fun (a, b, f) -> if a > b then 0 
                      else if a == b then f b 
                      else (f a) + sigma (a+1, b, f)