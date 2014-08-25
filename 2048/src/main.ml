
open Gg
open Vg
open React
open Useri

let log fmt = Format.printf (fmt ^^ "@.")

let board aspect = S.const (I.const Color.red)

let render_board r _ (aspect, board) =
  let view = Box2.v P2.o (Size2.v aspect 1.) in
  let renderable = `Image (Size2.unit, view, board) in
  ignore (Vgr.render r renderable);
  ()

let setup () =
  let aspect = S.map Size2.aspect Surface.size in
  let board = S.Pair.pair aspect (board aspect) in
  let c = Useri_jsoo.Surface.Handle.to_js (Surface.handle ()) in
  let r = Vgr.create (Vgr_htmlc.target ~resize:false c) `Other in
  App.sink_event (S.sample (render_board r) Surface.refresh board);
  Surface.(set_mode_setter @@ mode_flip (Key.up `Space));
  ()

let main () =
  let surface = Surface.create ~kind:`Other () in
  match App.init ~surface () with
  | `Error e -> Printf.eprintf "%s" e; exit 1
  | `Ok () -> setup (); App.run ()

let () = main ()
