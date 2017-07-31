let encode list =
  let rec help result count = function
  | [] -> []
  | [x] -> (count + 1, x) :: result
  | first :: (second :: _ as tail) ->
    if first = second
    then help result (count + 1) tail
    else help ((count + 1, first) :: result) 0 tail
  in List.rev (help [] 0 list);;