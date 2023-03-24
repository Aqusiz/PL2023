(* hw 2-5 of 2017-16140 *)
module type Queue = 
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end
module IntListQ =
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ: queue = ([], [])
    let enQ: queue * element -> queue = fun ((l, r), x) -> (x::l, r)
    let deQ: queue -> element * queue = fun q ->
      match q with
      | ([], []) -> raise EMPTY_Q
      | (llist, r::rlist) -> (r, (llist, rlist))
      | (llist, []) -> 
        let rec make_new_r l r = match l, r with
          | [], r -> r
          | hd::l, r -> make_new_r l (hd::r) in
        let new_r: element list = make_new_r llist [] in
        (List.hd new_r, ([], List.tl new_r))
  end