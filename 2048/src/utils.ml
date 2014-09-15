(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

(* (This is partial.) *)
let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | x -> List.(map hd x :: transpose (map tl x))

let fold_listlisti f acc ll =
  let col y (x, acc) square = (x + 1, f acc (x, y) square) in
  let row (y, acc) row = (y + 1, snd (List.fold_left (col y) (0, acc) row)) in
  snd (List.fold_left row (0, acc) (List.rev ll))

let listlist_dims ll =
  match ll with
  | [] -> (0, 0)
  | r :: _  -> (List.length r, List.length ll)

let rec replace_at n f l =
  match n, l with
    _, [] -> []
  | 0, x :: xs -> f x :: xs
  | n, x :: xs -> x :: replace_at (n - 1) f xs

let rec replace_one p l = match l with
| [] -> None
| x :: xs ->
    begin match p x with
    | Some y -> Some (y :: xs)
    | None ->
        begin match replace_one p xs with
        | None -> None
        | Some ys -> Some (x :: ys)
        end
    end
