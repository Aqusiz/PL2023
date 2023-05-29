(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)
type tyeqn =
  | Eqn of typ * typ
  | Sys of tyeqn * tyeqn

type gamma = var -> typ
type sol = typ -> typ

let (@+) f (x, v) = (fun y -> if y = x then v else f y)
let (@@+): sol -> sol -> sol = fun s1 s2 -> (fun t -> s1 (s2 t))
let rec (@=): typ -> typ -> bool = fun t1 t2 ->
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TString, TString -> true
  | TPair (e1, e2), TPair (e3, e4) -> (e1 @= e3) && (e2 @= e4)
  | TLoc l1, TLoc l2 -> l1 @= l2
  | TFun (e1, e2), TFun (e3, e4) -> (e1 @= e3) && (e2 @= e4)
  | TVar x1, TVar x2 -> x1 = x2
  | _ -> false

let emptyGamma: var -> typ = (fun x -> raise (M.RunError "unbound type"))
let emptySol: typ -> typ = (fun x -> x)
let add_sol: var -> typ -> sol = fun x t ->
  let rec sol' t' = 
    match t' with
    | TInt | TBool | TString -> t'
    | TPair (t1, t2) -> TPair (sol' t1, sol' t2)
    | TLoc l -> TLoc (sol' l)
    | TFun (t1, t2) -> TFun (sol' t1, sol' t2)
    | TVar x' -> if x = x' then t else t'
  in sol'
let rec v: gamma * M.exp * typ -> tyeqn = fun (g, e, t) ->
  match e with
  | M.CONST N _ -> Eqn (t, TInt)
  | M.CONST B _ -> Eqn (t, TBool)
  | M.CONST S _ -> Eqn (t, TString)
  | M.VAR x -> Eqn (t, g x)
  | M.FN (x, e') ->
    let alpha1 = new_var () in
    let alpha2 = new_var () in
    Sys (Eqn (t, TFun (TVar alpha1, TVar alpha2)),
        v (g @+ (x, TVar alpha1), e', TVar alpha2))
  | M.APP (e1, e2) ->
    let alpha = new_var () in
    Sys (v (g, e1, TFun (TVar alpha, t)),
        v (g, e2, TVar alpha))
  | M.IF (e1, e2, e3) ->
    Sys (Sys (v (g, e1, TBool), v (g, e2, t)), v (g, e3, t))
  | M.BOP (op, e1, e2) ->
    (match op with
      | M.ADD | M.SUB -> Sys (Sys (Eqn (t, TInt), v (g, e1, TInt)), v (g, e2, TInt))
      | M.AND | M.OR -> Sys (Sys (Eqn (t, TBool), v (g, e1, TBool)), v (g, e2, TBool))
      | M.EQ -> 
        let alpha = new_var () in
        Sys (Sys (Eqn (t, TBool), v (g, e1, TVar alpha)), v (g, e2, TVar alpha))
    )
  | M.READ -> Eqn (t, TInt)
  | M.WRITE e -> v (g, e, t)
  | M.PAIR (e1, e2) ->
    let alpha1 = new_var () in
    let alpha2 = new_var () in
    Sys (Sys (Eqn (t, TPair (TVar alpha1, TVar alpha2)), v (g, e1, TVar alpha1)), v (g, e2, TVar alpha2)) 
  | M.FST e ->
    let alpha = new_var () in
    v (g, e, TPair (t, TVar alpha))
  | M.SND e ->
    let alpha = new_var () in
    v (g, e, TPair (TVar alpha, t))
  | M.LET (d, e2) ->
    (match d with
      | M.VAL (x, e1) ->
        let alpha = new_var () in
        Sys (v (g, e1, TVar alpha), v (g @+ (x, TVar alpha), e2, t))
      | M.REC (f, x, e) ->
        let alpha1 = new_var () in
        let alpha2 = new_var () in
        Sys (v ((g @+ (f, TFun (TVar alpha2, TVar alpha1))) @+ (x, TVar alpha1), e, TVar alpha1), 
            v ((g @+ (f, TFun (TVar alpha2, TVar alpha1))) @+ (x, TVar alpha2), e2, t))
    )
  | M.MALLOC e ->
    let alpha = new_var () in
    Sys (Eqn (t, TLoc (TVar alpha)), v (g, e, TVar alpha))
  | M.ASSIGN (e1, e2) -> 
    Sys (v (g, e1, (TLoc t)), v (g, e2, t))
  | M.BANG e -> v (g, e, TLoc t)
  | M.SEQ (e1, e2) ->
    let alpha = new_var () in
    Sys (v (g, e1, TVar alpha), v (g, e2, t))

let rec appear: var -> typ -> bool = fun x t ->
  match t with
  | TVar x' -> x = x'
  | TInt | TBool | TString -> false
  | TPair (e1, e2) | TFun (e1, e2) -> (appear x e1) || (appear x e2)
  | TLoc l -> appear x l

let rec subst: sol -> tyeqn -> tyeqn = fun s u ->
  match u with
  | Eqn (t1, t2) -> Eqn (s t1, s t2)
  | Sys (u1, u2) -> Sys (subst s u1, subst s u2)

let rec typ_to_string: typ -> string = fun t ->
  match t with
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TString -> "TString"
  | TPair (e1, e2) -> "TPair (" ^ typ_to_string e1 ^ ", " ^ typ_to_string e2 ^")"
  | TLoc l -> "TLoc " ^ typ_to_string l
  | TFun (e1, e2) -> "TFun (" ^ typ_to_string e1 ^ ", " ^ typ_to_string e2 ^ ")"
  | TVar x -> "TVar " ^ x

let rec unify': typ -> typ -> sol = fun t1 t2 ->
  if t1 @= t2 then emptySol
  else (match t1, t2 with
    | TPair (e1, e2), TPair (e3, e4) -> 
      let s1 = unify' e1 e3 in
      let s2 = unify' e2 e4 in
      s1 @@+ s2
    | TLoc l1, TLoc l2 -> unify' l1 l2
    | TFun (e1, e2), TFun (e3, e4) ->
      let s1 = unify' e1 e3 in
      let s2 = unify' (s1 e2) (s1 e4) in
      s1 @@+ s2  
    | TVar x, _ -> if not (appear x t2) then add_sol x t2 else raise (M.TypeError "type error in unify' 1")
    | _, TVar x -> if not (appear x t1) then add_sol x t1 else raise (M.TypeError "type error in unify' 2")
    | _ -> 
      let _ = print_string (typ_to_string t1) in
      let _ = print_endline (" " ^ (typ_to_string t2)) in
      raise (M.TypeError "type error in unify' 3")
  )

let rec unify: tyeqn -> sol -> sol = fun u s ->
  match u with
  | Eqn (t1, t2) -> (unify' t1 t2) @@+ s
  | Sys (u1, u2) ->
    let t = unify u1 s in
    unify (subst t u2) t
(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let alpha = new_var () in
  let t = TVar alpha in
  let eqn = v (emptyGamma, exp, t) in
  let u = unify eqn emptySol in
  let rec transform: typ -> M.types = fun t ->
    match u t with
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (e1, e2) -> M.TyPair (transform e1, transform e2)
    | TLoc l -> M.TyLoc (transform l)
    | TFun (e1, e2) -> M.TyArrow (transform e1, transform e2)
    | TVar x -> raise (M.TypeError ("type error " ^ x))
  in transform t