let pstd = print_endline
let perr = prerr_endline

let fst (a,_) = a
let snd (_,b) = b
let length = List.length
let zip = List.combine
let for_all = List.for_all
let fold_left = List.fold_left
let fold_right = List.fold_right
let list_fold = fold_right
let fold_left2 = List.fold_left2
let map = List.map
let hd = List.hd
let tl = List.tl
let rev = List.rev
let filter = List.filter 
let flatten = List.flatten

let inc x = x + 1

let rec get_index : 'a -> 'a list -> int 
=fun x lst ->
  match lst with
  | [] -> raise (Failure "get_index: not found")
  | h :: t -> if x = h then 0 else 1 + get_index x t

let rec repeat e k = if k = 0 then [] else e::repeat e (k-1) 

let range n = 
  let rec helper n lst = 
    if n = 0 then rev lst
    else helper (n-1) ((n-1)::lst) in
  helper n []

let (|>) x f = f x
let uncurry f = fun (x,y) -> f x y
let (<<<) f g = fun x -> f (g x)
let (>>>) f g = fun x -> g (f x)
let ($>) x f = match x with Some s -> f s | None -> None
let (&>) x f = match x with Some s -> Some (f s) | None -> None
let (@) l1 l2 = BatList.append l1 l2
let id x = x
let flip f = fun y x -> f x y
let cond c f g x = if c then f x else g x
let opt c f x = if c then f x else x
let rec case cases default x =
  match cases with
  | (cond, f) :: t when cond -> f x
  | (_, _) :: t -> case t default x
  | [] -> default x
let tuple x = (x, x)

let compare_string = Base.compare_string
let compare_bool = Base.compare_bool
let compare_int = Base.compare_int

let domof m = BatMap.foldi (fun k _ set -> BatSet.add k set) m BatSet.empty

(** This applies [List.fold_left], but the argument type is the same with
    [PSet.fold].  *)
let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f list init ->
  List.fold_left (flip f) init list

let list_fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f list1 list2 init ->
  let f' acc a b = f a b acc in
  List.fold_left2 f' init list1 list2

let append_opt : 'a option -> 'a list -> 'a list
= fun x l ->
  match x with None -> l | Some x -> x::l
let find_opt : 'a -> ('a, 'b) BatMap.t -> 'b option
= fun k m ->
  try Some (BatMap.find k m) with
  | Not_found -> None

let find_def : 'a -> ('a, 'b) BatMap.t -> 'b -> 'b
= fun k m default ->
  try BatMap.find k m with _ -> default

let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first="[") ?(last="]") ?(sep=";") : ('a -> string)
  -> ('a list) -> string
= fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last

let string_of_set ?(first="{") ?(last="}") ?(sep=",") : ('a -> string)
  -> ('a BatSet.t) -> string
= fun string_of_v set ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ BatSet.fold add_string_of_v set "" ^ last

let string_of_map ?(first="{") ?(last="}") ?(sep=",\n") ?(indent="") : ('a -> string)
  -> ('b -> string) -> (('a, 'b) BatMap.t) -> string
= fun string_of_k string_of_v map ->
  let add_string_of_k_v k v acc =
    let str = string_of_k k ^ " -> " ^ string_of_v v in
    link_by_sep (sep^indent) str acc in
  if BatMap.is_empty map then "empty"
  else indent ^ first ^ BatMap.foldi add_string_of_k_v map "" ^ last

let i2s = string_of_int

let list2set l = list_fold BatSet.add l BatSet.empty
let set2list s = BatSet.fold (fun x l -> x::l) s []
