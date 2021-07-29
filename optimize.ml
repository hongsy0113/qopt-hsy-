open AST
open ASTUtils
open Vocab
open Pp
open Env

module Hello : sig
  val hello : unit -> unit
 end = 
 struct
   let message = "Hello"
   let hello () = print_endline message
 end
 
module Equiv =
  struct
  type t = (program, program) BatMap.t
  let empty = BatMap.empty
  let add = BatMap.add
end
(*
let x_propagation : program -> program
= fun smts-> 
  let equiv = Equiv.empty in
  let equiv = add []
*)

let optimize : program -> program 
=fun stmts -> stmts(*[Include ("test"); Decl(QReg("q", 1)); Qop(Uop(Gate("h", [], ["q", Some 1])))]*)
 (* TODO *)
