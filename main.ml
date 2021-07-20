open AST
open Vocab
open Options
open Analysis
open Optimize
open Pp

let _ = Random.self_init ()

let read_ast () = 
  let fname = 
    match !Options.input_file with
    | None -> raise (Failure "No input program given")
    | Some fname -> fname in
  let ast = OpenQASM.get_ast fname in
    ast

let main () = 
  let usage = "./main.native -input filename" in
  let _ = Arg.parse options (fun x -> ()) usage in
  let qc = Preprocess.run (read_ast ()) in
  let qc_opt = optimize qc in
    perr "\n** after optimization **"; pp_qc qc_opt;
    ()
let _ = main ()
