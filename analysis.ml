open AST
open ASTUtils
open Vocab
open Env

let count_qubits pgm = QEnv.sizeof (QEnv.init (collect_qregs pgm))

let rec count_gates pgm = 
  fold_left (fun count stmt -> 
    match stmt with
    | Qop qop -> count_gates_qop qop + count
    | If (_, _, qop) -> count_gates_qop qop
    | _ -> count
  ) 0 pgm 

and count_gates_qop qop = 
  match qop with
  | Uop (CX _) -> 1
  | Uop (U _) -> 1
  | Uop (Gate ("h", _, _)) -> 1
  | Uop (Gate ("cx", _, _)) -> 1
  | Uop (Gate ("x", _, _)) -> 1
  | Uop (Gate (_, _, _)) -> 0 (* TODO *)
  | _ -> 0

let print_qc_info fname ast = 
  pstd ("#Instrs: " ^ string_of_int (List.length ast));
  pstd ("#Qubits: " ^ string_of_int (count_qubits ast));
  pstd ("#Gates : " ^ string_of_int (count_gates ast))
