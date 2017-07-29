let rec at num = function
  | [] -> None
  | x :: rest -> if num = 1
    then Some x
    else at (num - 1) rest;;