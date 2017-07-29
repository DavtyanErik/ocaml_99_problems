let length list =
  let rec length k = function
    | [] -> k
    | head :: tail -> length (k + 1) tail
  in length 0 list;;