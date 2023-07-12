


let rec last l =
  match l with
  | [] -> None
  | e :: [] -> Some e
  | _ :: tl -> last tl