(* hw 2-1 of 2017-16140 *)
let rec iter: int * ('a -> 'a) -> 'a -> 'a
 = fun (n, f) a -> if n == 0 then a else iter(n - 1, f) (f a)