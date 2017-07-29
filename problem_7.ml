type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten list =
  let rec fill result = function
    | [] -> result
    | One head :: tail -> fill (head :: result) tail
    | Many head :: tail -> fill (fill result head) tail
  in List.rev (fill [] list);;