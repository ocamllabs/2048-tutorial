open Gg
open Vg

(* Colors *)

let square_colors square =
  let whiteish   = Color.v_srgbi 0xf9 0xf6 0xf2 in
  let brownish   = Color.v_srgbi 0x77 0x6e 0x65 in
  let background = Color.v_srgbi ~a:0.35 238 228 218 in
  match G2048.square_value square with
  | None      -> background, brownish
  | Some 2    -> Color.v_srgbi 0xee 0xe4 0xda, brownish
  | Some 4    -> Color.v_srgbi 0xed 0xe0 0xc8, brownish
  | Some 8    -> Color.v_srgbi 0xf2 0xb1 0x79, whiteish
  | Some 16   -> Color.v_srgbi 0xf5 0x95 0x63, whiteish
  | Some 32   -> Color.v_srgbi 0xf6 0x7c 0x5f, whiteish
  | Some 64   -> Color.v_srgbi 0xf6 0x5e 0x3b, whiteish
  | Some 128  -> Color.v_srgbi 0xed 0xcf 0x72, whiteish
  | Some 256  -> Color.v_srgbi 0xed 0xcc 0x61, whiteish
  | Some 512  -> Color.v_srgbi 0xed 0xc8 0x50, whiteish
  | Some 1024 -> Color.v_srgbi 0xed 0xc5 0x3f, whiteish
  | Some 2048 -> Color.v_srgbi 0xed 0xc2 0x2e, whiteish
  | _ -> assert false

let board_background = Color.v_srgbi 0xbb 0xad 0xa0
let msg_color = Color.v_srgbi 0x77 0x6e 0x65

(* Font *)

let base_font =
  { Font.name = "Arial"; slant = `Normal; weight = `W700; size = 1.0; }

let square_font box square =
  let count_digits n =                   (* number of decimal digits in n. *)
    let rec loop count n = if n = 0 then count else loop (count + 1) (n / 10) in
    if n = 0 then 1 else loop 0 n
  in
  let square_num = match G2048.square_value square with None -> 1 | Some n -> n in
  let digits = float (count_digits square_num) in
  let adjust = if digits > 2. then 1. /. (0.4 *. digits) else 1. in
  let size = 0.6 *. adjust *. Box2.h box in
  let offset = V2.v (-0.28 *. size *. digits) (-. 0.38 *. size) in
  { base_font with Font.size = size }, offset

(* Rendering squares *)

let image_of_square box square =
  let back, front = square_colors square in
  let square_font, offset = square_font box square in
  let square_label = G2048.string_of_square square in
  let label_origin = V2.(Box2.mid box + offset) in
  let label = I.cut_glyphs ~text:square_label square_font [] (I.const front) in
  I.cut P.(empty >> rect box) (I.const back) >>
  I.blend (label >> I.move label_origin)

(* Rendering boards *)

let full_overlay =
  let font = { base_font with Font.size = 0.15 } in
  let msg = I.cut_glyphs ~text:"Game Over!" font [] (I.const msg_color) in
  I.const (Color.gray ~a:0.4 0.9) >>
  I.blend (msg >> I.move (P2.v 0.08 0.47))

let image_of_board board = match G2048.board_size board with
| (0, 0) -> I.const board_background
| (w, h) ->
    let board_size = Size2.v (float w) (float h) in
    let pad_size = V2.(0.03 * (v (Size2.aspect board_size) 1.)) in
    let pad_cumulated_size = V2.(mul (Size2.unit + board_size) pad_size) in
    let square_size = V2.(div (Size2.unit - pad_cumulated_size) board_size) in
    let add_square acc (x, y) square =
      let ipos = P2.v (float x) (float y) in
      let pos = V2.(pad_size + mul ipos (pad_size + square_size)) in
      let box = Box2.v pos square_size in
      acc >> I.blend (image_of_square box square)
    in
    G2048.fold_board add_square (I.const board_background) board >>
    I.blend (if G2048.is_board_full board then full_overlay else I.void)
