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
let list_of_set: IntSet.t -> gift list = fun s -> List.of_seq (IntSet.to_seq s)
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
  let gifts = List.sort compare (list_of_set (make_gift_set require_list)) in
  let _ = print_endline (value_gift_list gifts) in
  (* make every possibe gift list (= powerset) *)
  let rec make_all_glist: gift list -> gift list list = fun g_list ->
    match g_list with
    | [] -> [[]]
    | g::g_tl -> 
      let subsets = make_all_glist g_tl in
      subsets @ List.map (fun subset -> g::subset) subsets
  in
  let gl_list = List.sort compare_list (make_all_glist gifts) in
  let _ = List.iter (fun gl -> print_string ("[" ^ value_gift_list gl ^ "] ")) gl_list in
  (* functions to check condition *)
  let rec eval_cond: (id * gift list) list -> id -> cond -> bool = fun l x c ->
    let my_gifts = IntSet.of_list (snd (List.find (fun ig -> (fst ig) = x) l)) in
    match c with
    | Items g_list -> IntSet.subset (IntSet.of_list g_list) my_gifts
    | Same id ->
      let target = IntSet.of_list (snd (List.find (fun ig -> (fst ig) = id) l)) in
      IntSet.subset my_gifts target
    | Common (c1, c2) -> (eval_cond l x c1) && (eval_cond l x c2)
    | Except (c, g_list) -> 
      let rec is_in: gift list -> gift list -> bool = fun l1 l2 ->
        match l2 with
        | [] -> true
        | g::tl -> (List.mem g l1) || is_in l1 tl 
      in
      (eval_cond l x c) && (not (is_in (list_of_set my_gifts) g_list))
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
  [(A, []);(B, []);(C, []);(D, []);(E, [])]

let req_list = 
  [(A, [Items [1;2];Common (Same B, Same C)]);
  (B, [Common (Same C, Items [2;3])]);
  (C, [Items [1];Except (Same A, [3])]);
  (D, []);(E, [])]
let _ = shoppingList req_list