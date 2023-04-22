(* hw 4-4 of 2017-16140 *)
(* dumb algorithm: make all cases possible and try *)
module IntSet = Set.Make(Int)
type require = id * (cond list)
and cond = 
Items of gift list
| Same of id
| Common of cond * cond
| Except of cond * gift list
and gift = int
and id = A | B | C | D | E
(*
(* helper functions for debug *)
open Printf
let value_id id = match id with
| A -> "A" | B -> "B" | C -> "C" | D -> "D" | E -> "E"
let value_bool b = if b then "true" else "false"
let rec value_cond cond = match cond with
| Items list -> "Items " ^ (String.concat " " (List.map string_of_int list))
| Same id -> "Same " ^ value_id id
| Common (cond1, cond2) -> "Common (" ^ value_cond cond1 ^ ", " ^ value_cond cond2 ^ ")"
| Except (cond, list) -> "Except (" ^ value_cond cond ^ ", " ^ (String.concat " " (List.map string_of_int list)) ^ ")"
let rec value_gift_list gift_list = match gift_list with
| [] -> ""
| gift :: rest -> string_of_int gift ^ " " ^ value_gift_list rest
let rec value_require require = match require with
| (id, list) -> value_id id ^ " " ^ (String.concat " " (List.map value_cond list))
let value_require_list require_list = String.concat " " (List.map value_require require_list)
let rec value_shopping_list shopping_list = match shopping_list with
| [] -> ""
| (id, list) :: rest -> value_id id ^ " " ^ (String.concat " " (List.map string_of_int list)) ^ " " ^ value_shopping_list rest
*)
let shoppingList: require list -> (id * gift list) list = fun require_list ->
  (* compare function for sorting list(asc) *)
  let compare: gift -> gift -> int = fun a b ->
    if a > b then 1
    else if a < b then -1
    else 0
  in
  let rec compare_list: gift list -> gift list -> int = fun l1 l2 ->
    if (List.length l1) > (List.length l2) then 1
    else if (List.length l1) < (List.length l2) then -1
    else (
      let (a, a_tl) = (List.hd l1, List.tl l1) in
      let (b, b_tl) = (List.hd l2, List.tl l2) in
      if a > b then 1
      else if a < b then -1
      else compare_list a_tl b_tl
    )
  in
  let list_of_set: IntSet.t -> gift list = fun s -> List.of_seq (IntSet.to_seq s) in
  (* make a set of all gifts *)
  let rec make_gift_set: require list -> IntSet.t = fun r_list -> match r_list with
  | [] -> IntSet.empty
  | (x, cond_list)::r_tl -> 
    let rec make_gift_set_: cond list -> IntSet.t = fun c_list -> match c_list with
    | [] -> IntSet.empty
    | c::c_tl -> 
      let rec eval_cond_: cond -> IntSet.t = fun c -> match c with
      | Items g_list -> IntSet.of_list g_list
      | Common (c1, c2) -> IntSet.union (eval_cond_ c1) (eval_cond_ c2)
      | Except (c, g_list) -> IntSet.diff (eval_cond_ c) (IntSet.of_list g_list)
      | Same _ -> IntSet.empty
      in
      IntSet.union (eval_cond_ c) (make_gift_set_ c_tl)
    in
    IntSet.union (make_gift_set_ cond_list) (make_gift_set r_tl)
  in
  let gifts = List.sort compare (list_of_set (make_gift_set require_list)) in
  (* make every possibe gift list (= powerset) *)
  let rec make_all_glist: gift list -> gift list list = fun g_list ->
    match g_list with
    | [] -> [[]]
    | g::g_tl -> 
      let subsets = make_all_glist g_tl in
      subsets @ List.map (fun subset -> g::subset) subsets
  in
  let make_sorted_gll: gift list -> gift list list = fun gl -> List.sort compare_list (make_all_glist gifts) in
  let gl_list = (make_sorted_gll gifts) in
  (* functions to check condition *)
  let rec cond_to_list: (id * gift list) list -> cond -> gift list =
    fun l c -> match c with
    | Items g_l -> g_l
    | Same x -> snd (List.find (fun ig -> (x = fst ig)) l)
    | Common (c1, c2) -> 
      let l1 = cond_to_list l c1 in
      let l2 = cond_to_list l c2 in
      List.filter (fun x -> List.mem x l2) l1
    | Except (c', g_l) ->
      let l1 = cond_to_list l c' in
      List.filter (fun x -> not (List.mem x g_l)) l1
  in
  let rec eval_cond: (id * gift list) list -> id -> cond -> bool = fun l x c ->
    let my_gifts = IntSet.of_list (snd (List.find (fun ig -> (fst ig) = x) l)) in
    match c with
    | Items g_list -> IntSet.subset (IntSet.of_list g_list) my_gifts
    | Same id ->
      let target = IntSet.of_list (snd (List.find (fun ig -> (fst ig) = id) l)) in
      IntSet.subset target my_gifts
    | Common (c1, c2) -> 
      let target = IntSet.of_list (cond_to_list l (Common (c1, c2))) in
      IntSet.subset target my_gifts
    | Except (c, g_list) -> 
      let rec is_in: gift list -> gift list -> bool = fun l1 l2 ->
        match l1 with
        | [] -> false
        | g::tl -> (List.mem g l2) || is_in tl l2
      in
      (eval_cond l x c) && (not (is_in g_list (list_of_set my_gifts)))
  in
  let rec eval_cond_list: (id * gift list) list -> id -> cond list -> bool = fun l x c_l ->
    match c_l with
    | [] -> true
    | c::tl -> (eval_cond l x c) && eval_cond_list l x tl
  in
  let eval_require: (id * gift list) list -> require -> bool = fun l (x, c_l) -> eval_cond_list l x c_l in
  let rec eval: (id * gift list) list -> require list -> bool = fun l r_l ->
    match r_l with
    | [] -> true
    | r::tl -> (eval_require l r) && (eval l tl)
  in
  (* check every cases and return (id * gift list) list that satisfy all conditions *)
  let rec sol: id -> gift list list -> require list -> (id * gift list) list -> (id * gift list) list =
    fun x gll rl acc -> match x, gll with
    | (_, []) -> [(A, []);(B, []);(C, []);(D, []);(E, [])]
    | (E, gl::tl) -> 
      let new_igll = acc @ [(E, gl)] in
      if eval new_igll rl then new_igll else (sol E tl rl acc)
    | (D, gl::tl) ->
      let new_igll = acc @ [(D, gl)] in
      if (eval (sol E gl_list rl new_igll) rl) then (sol E gl_list rl new_igll) else (sol D tl rl acc)
    | (C, gl::tl) ->
      let new_igll = acc @ [(C, gl)] in
      if (eval (sol D gl_list rl new_igll) rl) then (sol D gl_list rl new_igll) else (sol C tl rl acc)
    | (B, gl::tl) ->
      let new_igll = acc @ [(B, gl)] in
      if (eval (sol C gl_list rl new_igll) rl) then (sol C gl_list rl new_igll) else (sol B tl rl acc)
    | (A, gl::tl) ->
      let new_igll = acc @ [(A, gl)] in
      if (eval (sol B gl_list rl new_igll) rl) then (sol B gl_list rl new_igll) else (sol A tl rl acc)
    in
  sol A gl_list require_list []
(*
let req_list = 
  [(A, [Items [1;2];Common (Same B, Same C)]);
  (B, [Common (Same C, Items [2;3])]);
  (C, [Items [1];Except (Same A, [3])]);
  (D, []);(E, [])]
let _ = print_endline (value_shopping_list (shoppingList req_list))
*)