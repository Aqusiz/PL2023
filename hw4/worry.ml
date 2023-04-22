open Printf
module IntSet = Set.Make(Int)
type require = id * (cond list)
and cond = 
Items of gift list
| Same of id
| Common of cond * cond
| Except of cond * gift list
and gift = int
and id = A | B | C | D | E
(* helper functions for debug *)
let value_id id = match id with
| A -> "A" | B -> "B" | C -> "C" | D -> "D" | E -> "E"
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
(* compare function for sorting list(asc) *)
let compare: gift -> gift -> int = fun a b ->
  if a > b then 1
  else if a < b then -1
  else 0
let shoppingList: require list -> (id * gift list) list = fun require_list ->
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
  let gifts = List.sort compare (List.of_seq (IntSet.to_seq (make_gift_set require_list))) in
  let _ = print_endline (value_gift_list gifts) in
  [(A, []);(B, []);(C, []);(D, []);(E, [])]
(*
let req_list = 
  [(A, [Items [1;2];Common (Same B, Same C)]);
  (B, [Common (Same C, Items [2;3])]);
  (C, [Items [1];Except (Same A, [3])]);
  (D, []);(E, [])]
let _ = shoppingList req_list
*)