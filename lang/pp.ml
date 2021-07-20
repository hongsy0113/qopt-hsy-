open AST
open Vocab

exception Not_implemented

let rec pp_qc : program -> unit
=fun pgm -> 
  pstd "OPENQASM 2.0;";
  List.iter pp_stmt pgm

and pp_stmt : statement -> unit
=fun stmt -> 
  match stmt with
  | Include s -> pstd ("include " ^ "\"" ^ s ^ "\";")
  | Decl decl -> pstd (decl2str decl ^ ";")
  | GateDecl (gdecl, gops) -> pstd (gdecl2str gdecl gops)
  | OpaqueDecl gdecl -> raise Not_implemented
  | Qop qop -> pstd (qop2str qop ^ ";")
  | If (x, n, qop) -> pstd (if2str (x, n, qop) ^ ";")
  | Barrier args -> pstd (barrier2str args ^ ";")

and decl2str decl = 
  match decl with
  | QReg (x, n) -> ("qreg " ^ x ^ "[" ^ string_of_int n ^ "]")
  | CReg (x, n) -> ("creg " ^ x ^ "[" ^ string_of_int n ^ "]")

and gdecl2str (g, xs, qs) gops =
  ("gate " ^ g ^ " " ^ 
    string_of_list ~first:"(" ~last:")" ~sep:"," (fun x -> x) xs ^  
    string_of_list ~first:" " ~last:" " ~sep:"," (fun x -> x) qs ^
    "{ " ^ gops2str gops ^ " }")
       
and gops2str gops = 
  fold_right (fun gop s -> gop2str gop ^ "; " ^ s) gops ""

and gop2str gop = 
  match gop with
  | GUop uop -> uop2str uop
  | GBarrier _ -> ""

and qop2str qop = 
  match qop with
  | Uop uop -> uop2str uop
  | Meas (arg1, arg2) -> meas2str arg1 arg2
  | Reset arg -> ""

and uop2str uop =
  match uop with
  | CX (arg1, arg2) -> ("cx" ^ arg2str arg1 ^ ", " ^ arg2str arg2)
  | U (exps, arg) -> ""
  | Gate (g, exps, args) -> g ^ " " ^ 
      (if exps = [] then "" else string_of_list ~first:"(" ~last:")" ~sep:", " exp2str exps) ^ 
      (if exps = [] then "" else " ") ^ 
      (if args = [] then "" else string_of_list ~first:"" ~last:"" ~sep:", " arg2str args)

and if2str (x, n, qop) = "if (" ^ x ^ "==" ^ string_of_int n ^ ")" ^ " " ^ qop2str qop

and barrier2str args = "barrier " ^ string_of_list ~first:"" ~last:"" ~sep:"," arg2str args

and meas2str arg1 arg2 = "measure " ^ arg2str arg1 ^ " -> " ^ arg2str arg2 

and arg2str (x, idx) =
  match idx with
  | None -> x
  | Some i -> x ^ "[" ^ string_of_int i ^ "]"

and exp2str e = 
  match e with
  | Real f -> string_of_float f
  | Nninteger n -> string_of_int n
  | Pi -> "pi"
  | Id x -> x
  | BinaryOp (bop, e1, e2) -> "(" ^ exp2str e1 ^ ")" ^ bop2str bop ^ "(" ^ exp2str e2 ^ ")"
  | UnaryOp (unary, e) -> "(" ^ unary2str unary ^ exp2str e ^ ")" 
  | _ -> ""

and bop2str bop = 
  match bop with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Pow -> "pow" (* FIXME *)

and unary2str u =
  match u with
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Exp -> "exp"
  | Ln -> "ln"
  | Sqrt -> "sqrt"
  | UMinus -> "-"
