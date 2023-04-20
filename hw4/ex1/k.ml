(* hw 4-1 of 2017-16140 *)
(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | NUM n -> (Num n, mem)
    | UNIT -> (Unit, mem)
    | VAR x -> 
      let l = lookup_env_loc env x in
      (Mem.load mem l, mem)
    | ADD (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num (n1 + n2), mem'')
    | SUB (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num (n1 - n2), mem'')
    | MUL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num (n1 * n2), mem'')
    | DIV (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Num (n1 / n2), mem'')
    | EQUAL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let b = match v1, v2 with
        | Num n1, Num n2 -> n1 == n2
        | Bool b1, Bool b2 -> b1 == b2
        | Unit, Unit -> true
        | _ -> false in
      (Bool b, mem'')
    | LESS (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Bool (n1 < n2), mem'')
    | NOT e ->
      let (v, mem') = eval mem env e in
      let b = value_bool v in
      (Bool (not b), mem')
    | SEQ (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      (eval mem' env e2)
    | IF (e, e1, e2) ->
      let (v_cond, mem') = eval mem env e in
      let b = value_bool v_cond in
      if b then eval mem' env e1 else eval mem' env e2
    | WHILE (e1, e2) ->
      let (v_cond, mem') = eval mem env e1 in
      let b = value_bool v_cond in
      if b then 
        let (v1, mem1) = eval mem' env e2 in
        let (v2, mem2) = eval mem1 env (WHILE (e1, e2)) in
        (v2, mem2)
      else
        (Unit, mem')
    | LETF (f, x_list, e1, e2) ->
        eval mem (Env.bind env f (Proc (x_list, e1, env))) e2
    | CALLV (f, e_list) ->
        let (x_list, e', env') = lookup_env_proc env f in
        (* helper function: 
           store every id->addr & addr->value *)
        let rec set_premise mem env x_list e_list =
          match x_list, e_list with
          | [], [] -> (mem, env)
          | x::x_list, e::e_list ->
            let (v, mem') = eval mem env e in
            let (l, mem'') = Mem.alloc mem' in
            set_premise (Mem.store mem'' l v) (Env.bind env x (Addr l)) x_list e_list
          | _ -> raise (Error "InvalidArg")
        in
        let (mem'', env'') = set_premise mem env x_list e_list in
        (* Env need to bind f? *)
        (eval mem'' (Env.bind env'' f (Proc (x_list, e', env'))) e')
    | CALLR (f, y_list) ->
        let (x_list, e', _) = lookup_env_proc env f in
        (* helper function:
           match every x with x->sigma(y) *)
        let rec set_premise env x_list y_list =
          match x_list, y_list with
          | [], [] -> env
          | x::x_list, y::y_list ->
            let loc = match Env.lookup env y with
            | Addr l -> Addr l
            | _ -> raise (Error "")
            in
            set_premise (Env.bind env x loc) x_list y_list
          | _ -> raise (Error "InvalidArg")
        in
        let env' = set_premise env x_list y_list in
        (eval mem (Env.bind env' f (Proc (x_list, e', env'))) e')
    | RECORD xe_list ->
      if (List.length xe_list) == 0 then (Unit, mem)
      else
        let rec make_new_record mem env r xe_list = match xe_list with
        | [] -> r, mem
        | xe::xe_list ->
          let (x, e) = (fst xe, snd xe) in
          let (v, mem') = eval mem env e in
          let (l, mem'') = Mem.alloc mem' in
          let new_r = fun id -> if (compare id x) == 0 then l else r id in
          make_new_record (Mem.store mem'' l v) (Env.bind env x (Addr l)) new_r xe_list
        in
        let (new_record, mem') = make_new_record mem env (fun _ -> raise (Error "")) xe_list in
        (Record new_record, mem')
    | FIELD (e, x) ->
        let (v, mem') = eval mem env e in
        let r = value_record v in
        (Mem.load mem' (r x), mem')
    | ASSIGNF (e1, x, e2) ->
        let (v1, mem1) = eval mem env e1 in
        let r = value_record v1 in
        let (v2, mem2) = eval mem1 env e2 in
        (v2, (Mem.store mem2 (r x) v2))

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
