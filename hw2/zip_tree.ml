(* hw 2-4 of 2017-16140 *)
type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string
let goLeft loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")
let goRight: location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
let goDown: location -> location = fun loc ->
  match loc with
  | LOC(LEAF l, _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE n, zip) -> LOC(List.hd n, HAND([], zip, List.tl n))
let goUp: location -> location = fun loc ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) ->
    let node: tree list -> tree = fun tree_list -> NODE tree_list in
    let rec new_tree_list: tree list -> tree list -> tree list -> tree list 
    = fun lt t rt ->
      match lt, t, rt with
      | [], t, [] -> t
      | l::lt, t, [] -> new_tree_list lt (l::t) []
      | [], t, rt -> List.append t rt
      | lt, t, rt -> new_tree_list lt (List.append t rt) [] in
      LOC(node (new_tree_list left [t] right), up)