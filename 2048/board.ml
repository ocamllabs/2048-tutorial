open Gg
open Vg

(* The size and view of the canvas *)
let size = Size2.v 100.0 100.0
let view = Box2.unit

(* Font *)
let font = { Font.name = "Arial";
             slant = `Normal;
             weight = `W700;
             size = 0.11; }

(* Colours *)
let text = I.const (Color.v 0.18 0.13 0.16 1.0)
let dark_gray = I.const (Color.v 0.5 0.4 0.35 1.0)
let light_gray = I.const (Color.v 0.85 0.75 0.7 0.35)
let cream = I.const (Color.v 0.85 0.75 0.7 1.0)

let frame = P.empty >> P.rrect Box2.unit (Size2.v 0.02 0.02)

let square origin =
  P.rrect (Box2.v origin (Size2.v 0.22 0.22)) (Size2.v 0.02 0.02)

let square1 = square (P2.v 0.03 0.03)
let square2 = square (P2.v 0.03 0.27)
let square3 = square (P2.v 0.03 0.51)
let square4 = square (P2.v 0.03 0.75)

let square5 = square (P2.v 0.27 0.03)
let square6 = square (P2.v 0.27 0.27)
let square7 = square (P2.v 0.27 0.51)
let square8 = square (P2.v 0.27 0.75)

let square9 = square (P2.v 0.51 0.03)
let square10 = square (P2.v 0.51 0.27)
let square11 = square (P2.v 0.51 0.51)
let square12 = square (P2.v 0.51 0.75)

let square13 = square (P2.v 0.75 0.03)
let square14 = square (P2.v 0.75 0.27)
let square15 = square (P2.v 0.75 0.51)
let square16 = square (P2.v 0.75 0.75)

let squares =
  P.empty
  >> square1 >> square2 >> square3 >> square4
  >> square5 >> square6 >> square7 >> square8
  >> square9 >> square10 >> square11 >> square12
  >> square13 >> square14 >> square15 >> square16

let grid = I.blend (I.cut squares light_gray) (I.cut frame dark_gray)

let two_square =
  I.blend (I.move (V2.v 0.108 0.1) (I.cut_glyphs ~text:"2" font [] text))
          (I.cut (P.empty >> square1) cream)

let create c =
  Vgr.create (Vgr_htmlc.target c) `Other

let draw r =
  ignore (Vgr.render r (`Image (size, view, I.blend two_square grid)))

let destroy r =
  ignore (Vgr.render r `End)
