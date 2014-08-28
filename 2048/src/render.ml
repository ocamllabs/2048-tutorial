open Gg
open Vg

let base_font =
  { Font.name = "Arial"; slant = `Normal; weight = `W700; size = 1.0; }

(* Rendering squares *)

let square_color = Color.v_srgbi ~a:0.35 238 228 218
let square_img square_size =
  let square_box = Box2.v_mid P2.o square_size in
  I.cut P.(empty >> rect square_box) (I.const square_color)

(* Rendering tiles *)

let tile_colors num =
  (* color scheme stolen from http://gabrielecirulli.github.io/2048/ *)
  let whiteish_text = Color.v_srgbi 0xf9 0xf6 0xf2 in
  let brownish_text = Color.v_srgbi 0x77 0x6e 0x65 in
  match num with
  | 2    -> Color.v_srgbi 0xee 0xe4 0xda, brownish_text
  | 4    -> Color.v_srgbi 0xed 0xe0 0xc8, brownish_text
  | 8    -> Color.v_srgbi 0xf2 0xb1 0x79, whiteish_text
  | 16   -> Color.v_srgbi 0xf5 0x95 0x63, whiteish_text
  | 32   -> Color.v_srgbi 0xf6 0x7c 0x5f, whiteish_text
  | 64   -> Color.v_srgbi 0xf6 0x5e 0x3b, whiteish_text
  | 128  -> Color.v_srgbi 0xed 0xcf 0x72, whiteish_text
  | 256  -> Color.v_srgbi 0xed 0xcc 0x61, whiteish_text
  | 512  -> Color.v_srgbi 0xed 0xc8 0x50, whiteish_text
  | 1024 -> Color.v_srgbi 0xed 0xc5 0x3f, whiteish_text
  | 2048 -> Color.v_srgbi 0xed 0xc2 0x2e, whiteish_text
  | _ -> assert false

let tile_font tile_size num =
  let count_digits n =                     (* number of decimal digits in n. *)
    let rec loop count n = if n = 0 then count else loop (count + 1) (n / 10) in
    if n = 0 then 1 else loop 0 n
  in
  let digits = float (count_digits num) in
  let adjust = if digits > 2. then 1. /. (0.4 *. digits) else 1. in
  let size = 0.6 *. adjust *. Size2.h tile_size in
  let offset = V2.v (-0.28 *. size *. digits) (-. 0.38 *. size) in
  { base_font with Font.size = size }, offset

let base_tile_img ~scale tile_size num =
  let size = V2.(scale * tile_size) in
  let box = Box2.v_mid P2.o size in
  let color, text_color = tile_colors num in
  let font, offset = tile_font size num in
  let label = string_of_int num in
  let label = I.cut_glyphs ~text:label font [] (I.const text_color) in
  I.cut P.(empty >> rect box) (I.const color) >>
  I.blend (label >> I.move offset)

let drop_tile_img t square_size num = base_tile_img ~scale:t square_size num
let pop_tile_img t square_size num =
  let scale = Float.remap ~x0:0. ~x1:1. ~y0:1. ~y1:1.3 (Anim.tri t) in
  base_tile_img ~scale square_size num

let moving_tile_img t dpos square_size num =
  base_tile_img ~scale:1. square_size num >> I.move V2.((1. -. t) * dpos)

let moving_tiles t dir square_size square num =
  let move_time = Anim.ease_in_out t in
  let add_tile acc p =
    let dpos = V2.(float (-p.G2048.shift) * dir) in
    acc >> I.blend (moving_tile_img move_time dpos square_size p.G2048.value)
  in
  let pop_tile =
    if t < 0.8 then I.void else
    let t = Float.remap ~x0:0.8 ~x1:1.0 ~y0:0.0 ~y1:1.0 t in
    pop_tile_img (Anim.ease_in_out t) square_size num
  in
  List.fold_left add_tile I.void (G2048.square_provenances square) >>
  I.blend pop_tile

let tile_img t dir square_size square = match G2048.square_value square with
| None -> I.void
| Some num ->
    match G2048.square_provenances square with
    | [] -> drop_tile_img (Anim.ease_in_out t) square_size num
    | t :: [] -> base_tile_img ~scale:1. square_size num
    | tiles ->
        if t = 1. then base_tile_img ~scale:1. square_size num else
        moving_tiles t dir square_size square num

(* Rendering boards *)

let board_background = Color.v_srgbi 0xbb 0xad 0xa0

let move_direction = function
| None -> V2.zero
| Some move ->
    match move with
    | G2048.R -> V2.ox
    | G2048.L -> V2.(neg ox)
    | G2048.U -> V2.oy
    | G2048.D -> V2.(neg oy)

let board_img (w, h) t move board =
  let board_size = Size2.v (float w) (float h) in
  let pad_size = V2.(0.03 * (v (Size2.aspect board_size) 1.)) in
  let pad_cumulated_size = V2.(mul (Size2.unit + board_size) pad_size) in
  let square_size = V2.(div (Size2.unit - pad_cumulated_size) board_size) in
  let origin = V2.(pad_size + 0.5 * square_size) in
  let dir = V2.(mul (move_direction move) (pad_size + square_size)) in
  let add_square acc (x, y) square =
    let ipos = P2.v (float x) (float y) in
    let pos = V2.(origin + mul ipos (pad_size + square_size)) in
    let square_img = square_img square_size in
    let tile_img = tile_img t dir square_size square in
    let img = square_img >> I.blend tile_img in
    acc >> I.blend (img >> I.move pos)
  in
  G2048.fold_board add_square (I.const board_background) board

(* Overlays *)

let overlay_font = { base_font with Font.size = 0.15 }
let overlay_back_color = Color.gray ~a:0.4 0.9
let overlay_msg_color = Color.v_srgbi 0x77 0x6e 0x65

let overlay_img text text_pos =
  let msg = I.cut_glyphs ~text overlay_font [] (I.const overlay_msg_color) in
  I.const overlay_back_color >> I.blend (msg >> I.move text_pos)

let game_over_overlay = overlay_img "Game Over!" (P2.v 0.08 0.47)
let game_winning_overlay = overlay_img "You win!" (P2.v 0.2 0.47)

let board_overlay board =
  if G2048.is_board_winning board then game_winning_overlay else
  if G2048.is_game_over board then game_over_overlay else
  I.void

(* Rendering boards *)

let animate_board t move board = match G2048.board_size board with
| (0, 0) -> I.const board_background
| size -> board_img size t move board >> I.blend (board_overlay board)

let image_of_board board = animate_board 1.0 None board
