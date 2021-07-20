open AST
open Vocab
open Env

let collect_qregs stmts = 
  let collect stmt lst = 
    match stmt with
    | Decl (QReg (x, s)) -> (x, s)::lst
    | _ -> lst
  in list_fold collect stmts []

let collect_gates stmts = 
  let collect stmt gates = 
    match stmt with
    | GateDecl ((g, xs, qs), gops) -> Gates.add g (xs, qs, gops) gates
    | _ -> gates
  in list_fold collect stmts Gates.empty


