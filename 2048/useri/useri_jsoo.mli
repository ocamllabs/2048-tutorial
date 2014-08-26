(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Useri [js_of_ocaml] backend specific functions. *)

open Gg
open React

(** User keyboard.

    Consult information about {{!get}getting keyboard events} and
    {{!limits}limitations}. *)
module Key : sig

(** {1:src Keyboard event target} *)

  val event_target : unit -> Dom_html.eventTarget Js.t option
  (** [source ()] is the event target used for keyboard events. *)

  val set_event_target : Dom_html.eventTarget Js.t option -> unit
  (** [set_event_target target] sets the event target to target. If [None]
      the canvas of the surface will be used.

      {b Warning.} This may be removed in the future.

      {b Important.} You need to set the event target before
      calling {!Useri.App.init}. And a {!Useri.App.release} sets
      the event target back to [None]. *)

(** {1:capture Keyboard event capture} *)

  val key_capture : unit -> (Useri_base.Key.id -> bool)
  (** [key_capture ()] is the function that determines whether key
      events related to a key are captured by Useri (i.e prevent
      default action and stop propagation). The initial function never
      captures. *)

  val set_key_capture : (Useri_base.Key.id -> bool) -> unit
  (** [set_capture_key capture] sets the function returned
      by {!key_capture}. *)

(** {1:get Getting keyboard events}

    There are a few things you need to make sure are setup in order
    to get the keyboard events.

    First to get events directly on the HTML canvas associated to the
    surface it needs to have a [tabindex] attribute. If [Useri]
    creates the canvas it sets one, but don't forget to set one if you
    provide the canvas at initialization time through an
    {{!Surface.Handle.of_js}handle}. Events will only be generated
    once the user has focused the canvas in one way or another
    (e.g. by clicking on it). The latter operation may introduce a
    selection box around the canvas, the selection box can be hidden
    by applying the CSS rule [{ outline: none; }] on the canvas.

    You can also choose to get the keyboard events from another event
    target using the {!set_event_target} before initializing the
    application. For example using the {!Dom_html.window} will prevent
    the user from having to focus in order for you to get keyboard
    events.

    By default keyboard event will not stop propagating and will perform
    their default action. You can prevent that to occur on certain keys
    by registering a function with {!set_key_capture}. Note that
    in certain browsers (e.g. Safari) it is not possible to prevent
    the default action of certain key strokes.

    {b Fullscreen.} Note that in general in fullscreen mode you may
    not get keyboard events or only some of them for security reasons.

    {1:limits Limitations}

    The following limitations exist (they may be lifted in the future).
    {ul
    {- The backend cannot distinguish between [`Enter] and
      [`Return] keys. [`Return] is always returned.}
    {- The backend cannot distinguish between [`Left] and [`Right] keys for
       modifiers [`Alt, `Ctrl, `Meta] and [`Shift]. It is advised to use
       handless {!Useri.Key.alt}, {!Useri.Key.ctrl}, {!Useri.Key.meta} and
       {!Useri.Key.shift} for detecting modifiers.}}
*)
end

(** User drag and drop. *)
module Drop : sig

  module File : sig
    (** {1 JavaScript file object} *)

    val to_js : Useri_base.Drop.file -> File.file Js.t
    (** [to_js f] returns the underlying JavaScript file object. *)
  end

  (** {1:limits Limitations}

      The current browser {{:http://www.w3.org/TR/FileAPI/}File API} doesn't
      allow to detect if a file drop is a directory or not. Trying to
      {!Useri.Drop.File.prepare} a directory will result in an error. *)
end

(** Surface.


*)
module Surface : sig

  (** {1:smode Surface mode}

      [`Fullscreen] mode uses JavaScript's
      {{:http://www.w3.org/TR/fullscreen/}Fullscreen API} and applies
      it to the document element rather than the surface itself. This
      is the most flexible as it allows you to keep surrounding
      elements around the surface or hide them using the CSS
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/:fullscreen}
      fullscreen} pseudo-class. *)

  (** {1:sspec Surface specification}

      Take into account the following points:
      {ul
      {- The [pos] argument of {!Useri.Surface.create} is ignored.}
      {- If the [size] argument is unspecified. The surface's size
         is not set explicitely but inherited
         from the document itself (e.g. through CSS).}} *)

  (** {1:propsupdate Surface properties updates}

      Browser lack the ability to detect element bounding box
      changes as such {!Useri.Surface.pos}, {!Useri.Surface.raster_size} and
      {!Useri.Surface.size} only get updated, if needed, on
      {!Useri.Surface.mode} changes and window resizes. If that
      doesn't handle all your needs you can call {!request_sync_props}
      to request the surface properties to be synchronized. *)

  val request_sync_props : unit -> unit
  (** [request_sync_props ()] asks to make sure that the application's
      surface {!Useri.Surface.pos}, {!Useri.Surface.raster_size} and
      {!Useri.Surface.size} properties are in sync with the surface's
      element bounding box some time after it was called (can be
      called in a React step). *)

  val size_mm : size2 signal
  (** [size_mm] is the application's surface size in millimeters. *)

  (** {1:handles Handle} *)

  (** Surface handles. *)
  module Handle : sig
    val of_js : Dom_html.canvasElement Js.t -> Useri_base.Surface.handle
    val to_js : Useri_base.Surface.handle -> Dom_html.canvasElement Js.t
  end
end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
