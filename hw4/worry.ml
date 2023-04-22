open Printf
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
let shoppingList: require list -> (id * gift list) list = fun require_list ->
  
  [(A, []);(B, []);(C, []);(D, []);(E, [])]