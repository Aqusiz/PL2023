(* hw 1-3 of 2017-16140 *)
type crazy3 = NIL | ZERO of crazy3 | ONE of crazy3 | MONE of crazy3 | TWO of crazy3 | MTWO of crazy3
let rec c3v: crazy3 * int * int -> int = function
| (NIL, k, s) -> s
| (ZERO c, k, s) -> c3v(c, k*3, s)
| (ONE c, k, s) -> c3v(c, k*3, s + k)
| (MONE c, k, s) -> c3v(c, k*3, s - k)
| (TWO c, k, s) -> c3v(c, k*3, s + 2*k)
| (MTWO c, k, s) -> c3v(c, k*3, s - 2*k)
let rec crazy3val: crazy3 -> int = fun c -> c3v(c, 1, 0)