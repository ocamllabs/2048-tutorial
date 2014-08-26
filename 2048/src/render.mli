(** 2048 game board renderer *)

(** {1 Rendering boards} *)

val image_of_board : G2048.board -> Vg.image
(** [image_of_board b] is an image from [board]. *)
