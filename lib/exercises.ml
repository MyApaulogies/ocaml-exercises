(* these are the solutions to https://ocaml.org/problems?difficulty_level=All 
   which I have simply tested by pasting into `utop` so far
   this file was basically written in order *)

let rec last l =
  match l with
  | [] -> None
  | [e] -> Some e
  | _ :: tl -> last tl


let rec last_two l =
  match l with 
  | [] -> None
  | [a; b] -> Some (a, b)
  | _ :: tl -> last_two tl


(* I am actually a wizard *)
let rec nth l n =
  match (n, l) with
  | (_, []) -> None
  | (0, hd :: _) -> Some hd
  | (n, _ :: tl) -> nth tl (n-1)


let rec len l =
  let rec lenctr l n =
    match l with
    | [] -> n
    | _hd :: tl -> lenctr tl (n+1)
  in lenctr l 0


let reverse l = 
  let rec revbuilder orig other =
    match orig with
    | [] -> other
    | hd :: tl -> revbuilder tl (hd :: other)
  in revbuilder l []


let is_palindrome l =
  l = reverse l



type 'a node =
| One of 'a 
| Many of 'a node list


let nodeflatten l = 
  let rec f res l =
    match l with
    | [] -> res
    | One x :: tl -> f (x :: res) tl
    | Many xs :: tl -> f ((f [] xs) @ res) tl
  in f [] l |> reverse


(* attempt 2 after seeing `compress` solution + strategy (this is so fucking cool) *)
let rec nodeflatten2 = function
  | One x :: tl -> x :: nodeflatten2 tl
  | Many xs :: tl -> nodeflatten2 xs @ nodeflatten2 tl
  | [] -> []


(* let rec listflatten = function
| (_ :: _ as sublist) :: tl -> listflatten sublist @ listflatten tl
| hd :: tl -> hd :: listflatten tl
| [] -> [] *)


let remdup l = 
  let rec f res l = 
    match l with
    | next :: tl -> (
      match res with
      | Some x -> (
        match x with
        | prev :: _ ->
          if prev = next then
            f res tl
          else
            f (Some (next :: x)) tl)
      | None -> f (Some [next]) tl)
    | [] -> 
      match res with
      | None -> []
      | Some r -> r
  in f None l |> reverse

(* after seeing solution *)
let rec dedup = function
  | a :: (b :: _ as tl) -> if a = b then dedup tl else a :: dedup tl
  | _ -> []



let rec groupdup l = 
  let rec f res l =
    match (res, l) with
    | ((prev :: _ as currlist) :: res_tl, elem::tl) -> 
      let newres = 
        if elem = prev then
          (elem::currlist)::res_tl
        else
          [elem]::res
      in f newres tl
    | ([], elem::tl) -> f [[elem]] tl (* start *)
    | (_, []) -> res (* end *)
    | (_, _) -> [[]] (* unreachable *)
  in f [] l |> reverse

(* python version (helped me figure it out)

def groupdup(lis):
  res = []
  prev = None
  for elem in lis:
    if elem == prev:
      res[-1].append(elem)
    else:
      res.append([elem])
    prev = elem
  return res
*)

(* less complicated (not really) *)
let groupdup2 l = 
  let rec f res = function
  | [] -> []
  | elem :: tl -> 
    let newres = 
      match res with
      | (prev :: _ as currlist) :: res_tl -> 
        if prev = elem then
          (prev :: currlist) :: res_tl
        else
          [elem] :: res
      | [] -> [elem] :: res
      | _ -> [[];[]] (*unreachable*)
    in f newres tl
  in f [] l |> reverse


(* saw solution *)
let groupdup3 l = 
  let rec f sub res = function
    | a :: (b :: _ as tl) -> 
      if a = b then
        f (a :: sub) res tl
      else
        f [] ((a::sub)::res) tl
    | a :: [] -> (a :: sub) :: res
    | [] -> []
  in f [] [] l |> reverse


let encode1 l = 
  let rec f ctr prev res l =
    match l with
    | [] -> (ctr, prev) :: res
    | elem :: tl ->
      if elem = prev then f (ctr + 1) prev res tl
      else f 1 elem ((ctr, prev) :: res) tl
  in reverse (match l with
  | elem :: tl -> f 1 elem [] tl
  | [] -> [])


let encode2 l =
  l 
  |> groupdup2 
  |> List.map (fun (elem :: tl) -> (1 + len tl, elem))


let rec encode3 l =
  let combine e l =
    match l with
    | [] -> [] (* never happens *) 
    | (ctr, prev) :: tl -> 
      if prev = e then
        ((ctr + 1), prev) :: tl
      else
        (1, e) :: l
  in match l with
  | [] -> [] 
  | a :: [] -> [(1, a)]
  | a :: tl -> combine a (encode3 tl)


let rec encode4 l =
  let combine e l =
    match l with
    | [] -> [(1, e)]
    | (ctr, prev) :: tl -> 
      if prev = e then
        ((ctr + 1), prev) :: tl
      else
        (1, e) :: l
  in match l with
  | [] -> [] 
  | a :: tl -> combine a (encode4 tl)


type 'a rle = | One of 'a | Many of int * 'a

let rec rle_encode l =
  let combine e l =
    match l with
    | [] -> [One e]
    | One prev :: tl when e = prev -> Many (2, prev) :: tl
    | Many (ctr, prev) :: tl when e = prev -> Many (ctr + 1, prev) :: tl
    | _ -> One e :: l
  in
  match l with
  | [] -> []
  | a :: tl -> combine a (rle_encode tl)


let rec multi_prepend times e l = 
  match times with
  | 0 -> l
  | times -> e :: multi_prepend (times - 1) e l

let rec rle_decode l =
  match l with 
  | One e :: tl -> e :: rle_decode tl
  | Many (ctr, a) :: tl -> multi_prepend ctr a (rle_decode tl)
  | [] -> []



let rec dup = function
  | [] -> []
  | e :: tl -> e :: e :: dup tl


let rec rep l times = 
  match l with
  | [] -> []
  | e :: tl -> multi_prepend times e (rep tl times)



let drop1 l n =
  let rec f ctr acc l =
    match l with
    | [] -> acc
    | e :: tl -> 
      if ctr = n then
        f 1 acc tl
      else
        f (ctr + 1) (e :: acc) tl
  in
  f 1 [] l |> reverse


(* saw solution *)
let drop2 l n =
  let rec f ctr l =
    match l with
    | [] -> []
    | _ :: tl when ctr = n -> f 1 tl
    | e :: tl -> e :: f (ctr + 1) tl
  in
  f 1 l


(* new, successful attempt (lets go) *)
let rec listflatten2 = function
  | [] -> []
  | sublist :: tl -> sublist @ listflatten2 tl



(* weird -- do something car on left, then recurse, then cons on right *)
(* this one took a pen and paper *)
let split l length =
  let rec f n acc l = 
    if n = 0 then
      (acc, l)
    else
      match l with
      | [] -> (acc, [])
      | e :: tl -> 
        let (left, right) = f (n-1) acc tl 
        in 
        (e :: left, right)
  in
  f length [] l


(* saw solution *)
let split2 l length =
  let rec f n acc l = 
    if n = 0 then
      reverse acc, l
    else
      match l with
      | [] -> (acc, [])
      | e :: tl -> f (n-1) (e::acc) tl
  in
  f length [] l


let rec slice l start stop = 
  match l with
  | [] -> []
  | e :: tl -> 
    if start > 0 then
      slice tl (start-1) (stop-1)
    else if stop > 0 then
      (* now ignore start *) 
      e :: slice tl 0 (stop-1)
    else
      [e]


(* after seeing solution *)
let rec drop n l =
  match l, n with
  | [], _ -> []
  | _, 0 -> l
  | _ :: tl, _ -> drop (n-1) tl

let rec take n l =
  match l, n with
  | [], _ | _, 0 -> []
  | e :: tl, _ -> e :: take (n-1) tl

let rec slice2 l start stop = 
  l |> drop start |> take (stop-start+1)



let rec rotate l n =
  (l |> drop n) @ (l |> take n)


let rec remove_at i l =
  match i, l with
  | _, [] -> []
  | 0, _ :: tl -> tl
  | _, e :: tl -> e :: remove_at (i-1) tl


let rec insert_at e i l =
  match i, l with
  | 0, _ -> e :: l
  | _, [] -> [e]
  | _, hd::tl -> hd :: insert_at e (i-1) tl