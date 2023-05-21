(* hw 6-1 of 2017-16140 *)
(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.PUT]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Unit))]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e1 -> trans e1 @ [Sm5.NOT]
    | K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.ASSIGN (id, e) -> trans e @ [Sm5.PUSH (Sm5.Id id); Sm5.STORE]
    | K.SEQ (e1, e2) -> trans e1 @ trans e2
    | K.IF (e_c, e_t, e_f) -> trans e_c @ [Sm5.JTR (trans e_t, trans e_f)]
    | K.LETF (f, x, e1, e2) -> 
      [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans e1)); Sm5.BIND f] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.WHILE (e_cond, e_body) -> 
      trans (K.LETF ("_", 
                    "__", 
                    K.IF (
                      e_cond, 
                      K.SEQ(e_body, K.CALLV ("_", K.UNIT)),
                      K.UNIT),
                    K.CALLV ("_", K.UNIT)
                    )
            )
    | K.FOR (id, e1, e2, body) ->
      trans (K.LETV (id, 
                      e1,
                      K.WHILE (
                        K.NOT (K.LESS (e2, K.VAR id)), 
                        K.SEQ (body, K.ASSIGN (id, K.ADD (K.VAR id, K.NUM 1)))
                      )
                    )
            )
    | K.CALLV (f, arg_exp) ->
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, arg_var) ->
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id arg_var); Sm5.LOAD; Sm5.PUSH (Sm5.Id arg_var); Sm5.CALL]
end
