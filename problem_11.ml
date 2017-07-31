type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode list =
  let one_or_many count element =
    if count = 1 then One element else Many (count, element) in
  let rec help result count = function
  | [] -> []
  | [x] -> one_or_many (count + 1) x :: result
  | first :: (second :: _ as tail) ->
    if first = second
    then help result (count + 1) tail
    else help (one_or_many (count + 1) first :: result) 0 tail
  in List.rev (help [] 0 list);;