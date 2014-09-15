(*
 * Copyright (c) 2014 Daniel C. Bünzli.
 *
 * This file is distributed under the terms of the BSD3 License.
 * See the file COPYING for details.
 *)

open Gg
open Vg
open React
open Useri

let log fmt = Format.printf (fmt ^^ "@.")

let user_move : G2048.move event =
  let u = E.stamp (Key.up (`Arrow `Up))    G2048.U in
  let d = E.stamp (Key.up (`Arrow `Down))  G2048.D in
  let l = E.stamp (Key.up (`Arrow `Left))  G2048.L in
  let r = E.stamp (Key.up (`Arrow `Right)) G2048.R in
  E.select [u; d; l; r]

let board : G2048.board signal =
  let move m board =
    if G2048.is_board_winning board || G2048.is_game_over board
    then G2048.create_board ()
    else G2048.game_move m board
  in
  S.accum (E.map move user_move) (G2048.create_board ())

let t : float signal = (* on each user_moves goes from 0. to 1. in ~span secs *)
  let transition = E.map (fun _ _ -> Time.unit ~span:0.3) user_move in
  S.switch (S.accum ~eq:(==) transition (S.const 1.))

let last_user_move : G2048.move option signal =
  S.hold None (E.Option.some user_move)

let display : (Size2.t * Vg.image) signal =
  let img = S.l3 Render.animate_board t last_user_move board in
  S.Pair.pair Surface.size img

let setup () =
  let render_display r _ (size, img) =
    let renderable = `Image (size, Box2.unit, img) in
    ignore (Vgr.render r renderable);
    ()
  in
  let c = Useri_jsoo.Surface.Handle.to_js (Surface.handle ()) in
  let r = Vgr.create (Vgr_htmlc.target ~resize:false c) `Other in
  Surface.set_refresher user_move;
  App.sink_event (S.sample (render_display r) Surface.refresh display);
  ()

let main () =
  let key_target = Some (Dom_html.window :> Dom_html.eventTarget Js.t) in
  let surface = Surface.create ~kind:`Other () in
  Useri_jsoo.Key.set_event_target key_target;
  match App.init ~surface () with
  | `Error e -> Printf.eprintf "%s" e; exit 1
  | `Ok () -> setup (); App.run ()

let () = main ()
