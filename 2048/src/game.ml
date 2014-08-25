(** 2048 game interface *)

let onload _ =
  (* Set up the screen, install event handlers, and start the game *)
  let main = Dom_html.getElementById "main" in
  let heading = Dom_html.createH2 Dom_html.document in
  let canvas = Dom_html.createCanvas Dom_html.document in
    main##style##width <- Js.string "100mm";
    main##style##margin <- Js.string "0 auto";
    heading##innerHTML <- Js.string "2048";
    heading##style##textAlign <- Js.string "center";
    Dom.appendChild main heading;
    Dom.appendChild main canvas;
    let renderer = Vg.Vgr.create (Vg.Vgr_htmlc.target canvas) `Other in
      Board.draw renderer

    let b = Board.create canvas in
      Board.draw b;
      Board.destroy b;
      Js._false

let _ = Dom_html.window##onload <- Dom_html.handler onload
