let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> last_two rest;;