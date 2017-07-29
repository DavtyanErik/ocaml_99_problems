let is_palindrome list =
  let reverse list =
    let rec fill_reverse result = function
      | [] -> result
      | head :: tail -> fill_reverse (head :: result) tail
    in fill_reverse [] list
  in list = reverse list;;