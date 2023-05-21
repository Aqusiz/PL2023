let r = \n.\f.\x.f (n f x) in
let a = \f.\x.(f x) in
let b = \f.\x.(f(f x)) in
let s = \m.\n.(m r n) in
s(a b)