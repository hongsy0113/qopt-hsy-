open AST
open Vocab

module Gates = struct
  type t = (id, (id list * id list * gop list)) BatMap.t
  let empty = BatMap.empty
  let add = BatMap.add
  let find g t = 
    try BatMap.find g t 
    with _ -> raise (Failure ("Gates.find: " ^ g))
end

module QEnv = struct
  type addr = int
  type size = int
  type t = (id, addr * size) BatMap.t
  let empty = BatMap.empty

  let bind var addr env = BatMap.add var (addr, 1) env

  let rec find (var,idx) env = 
    try 
      let (addr, size) = BatMap.find var env in
      let _ = assert (idx < size) in
        addr + idx
    with _ -> raise (Failure "QEnv.find")

  let addr_of_qbit : t -> argument -> addr
  =fun env arg ->
    match arg with
    | (q, None) -> find (q, 0) env
    | (q, Some i) -> find (q, i) env 
    | _ -> raise (Failure "addr_of_qbit")

  let qreg_of_addr : t -> addr -> id * int
  =fun env addr -> 
    let (q, (a,s)) = 
      BatMap.find_first (fun q -> 
        let (a,s) = (BatMap.find q env) in
          a <= addr && addr < a + s
      ) env in
      (q, addr - a)
   
     
  let init vars = 
    list_fold (fun (x,addr,size) -> BatMap.add x (addr,size)
    ) (fst (fold_left (fun (env,addr) (x,size) -> ((x,addr,size)::env, addr+size)) ([],0) vars))
    empty

  let qregs_of env = BatMap.foldi (fun x (_,size) lst -> (x,size)::lst) env []

  let sizeof env = BatMap.foldi (fun _ (_,s) a -> s + a) env 0

  let print env = 
    print_endline ("size: " ^ string_of_int (sizeof env));
    BatMap.iter (fun x  (addr, size) ->
      print_endline (x ^ " |-> (" ^ string_of_int addr ^ ", " ^ string_of_int size ^ ")")
    ) env
end

