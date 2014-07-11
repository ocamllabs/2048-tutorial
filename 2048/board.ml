open Gg
open Vg

let size = Size2.v 100.0 100.0
let view = Box2.unit

let rect = P.empty >> P.rrect Box2.unit (Size2.v 0.02 0.02)
let grey = I.const (Color.v_srgbi 187 173 160)
let background = I.cut rect grey

let create c =
  Vgr.create (Vgr_htmlc.target c) `Other

let draw r =
  ignore (Vgr.render r (`Image (size, view, background)))

let destroy r =
  ignore (Vgr.render r `End)
