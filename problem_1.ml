let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest;;