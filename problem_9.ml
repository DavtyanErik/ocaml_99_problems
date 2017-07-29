let pack list =
  let rec _pack result current = function
    | [] -> current :: result
    | head :: tail ->
      match current with
      | [] -> _pack result [head] tail 
      | hd :: _ -> 
        if hd = head 
          then _pack result (head :: current) tail
          else _pack (current :: result) [head] tail
  in List.rev (_pack [] [] list);;