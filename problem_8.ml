let compress list =
  let rec compress result = function
    | first :: (second :: _ as rest) -> if first = second
      then compress result rest
      else compress (first :: result) (second :: rest)
    | head :: [] -> head :: result
    | [] -> result
    in List.rev (compress [] list);;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;