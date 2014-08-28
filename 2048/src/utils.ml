(* (This is partial.) *)
let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | x -> List.(map hd x :: transpose (map tl x))
