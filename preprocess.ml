open AST
open ASTUtils
open Vocab
open Pp

let remove_cx_and_u stmts = 
  map (fun stmt ->
    match stmt with
    | Qop (Uop (CX (arg1, arg2))) -> Qop (Uop (Gate ("cx", [], [arg1; arg2])))
    | Qop (Uop (U (exps, arg))) -> Qop (Uop (Gate ("u", exps, [arg])))
    | _ -> stmt
  ) stmts

let inline stmts = stmts (* TODO *)

let run stmts = 
  stmts 
  |> inline
  |> remove_cx_and_u
