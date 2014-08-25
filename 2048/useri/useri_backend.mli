(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Simple declarative user interaction with {!React}.

    [Useri] gathers user input as {!React} signals and events from
    a single rendering surface.

    Open the module to use it, this defines only modules in your scope.

    {b Note.} Before {!App.init} is called all signals hold invalid data.

    {b Caveat.} Do not expect to be able to fully exploit the
    possibilities and flexibility of the platforms underlying the
    backends. This library is a {e simple} abstraction library and
    thus remains limited by design.

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

open Gg
open React

(** {1 Useri} *)

(** Monotonic time. *)
module Time : sig

  (** {1 Time span} *)

  type span = float
  (** The type for time spans, in seconds. *)

  (** {1 Passing time} *)

  val elapsed : unit -> span
  (** [elapsed ()] is the number of seconds elapsed since the
      beginning of the program. *)

  val tick : span -> span event
  (** [tick span] is an event that occurs once in [span] seconds with
      the value [span - span'] where [span'] is the actual delay
      performed by the system.

      {b Note.} Since the system may introduce delays you cannot
      assume that two different calls to {!tick} will necessarily
      yield two non-simultaneous events. *)

  (** {1 Timing animations}

      These signals are for coordinating animation and surface
      refreshes. See {!rendercoord} for more information. *)

  val count : until:'a event -> span signal
  (** [count ~until] is a signal that counts time starting from [0.]
      until the next occurence of [until]. *)

  val unit : span:span -> float signal
  (** [unit ~span] is a signal that linearly increases from [0.] to [1.]
      during [span] seconds. *)

  (** {1 Counters} *)

  type counter
  (** The type for time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting time from call time on. *)

  val counter_value : counter -> span
  (** [counter_value c] is the current counter value in seconds. *)

  (** {1 Pretty printing time} *)

  val pp_s : Format.formatter -> span -> unit
  (** [pp_s ppf s] prints [s] seconds in seconds. *)

  val pp_ms : Format.formatter -> span -> unit
  (** [pp_ms ppf s] prints [s] seconds in milliseconds. *)

  val pp_mus : Format.formatter -> span -> unit
  (** [pp_mus ppf s] prints [s] seconds in microseconds. *)
end

(** Rendering surface.

    An application has a single rendering surface that
    is specified by a {!t} value given to {!App.init}. *)
module Surface : sig

  (** {1:smode Surface mode}

      See also {{!Useri_jsoo.Surface.smode}surface mode information}
      in the [`Jsoo] backend. *)

  type mode = [ `Windowed | `Fullscreen ]
  (** The type for surface modes. *)

  val mode : mode signal
  (** [mode] is the application's surface mode. The value of this
      signal is defined by the [~mode] argument of the surface given
      to {!App.init}, the occurences of the event specified by
      {!set_mode_setter} and external events. *)

  val set_mode_setter : mode event -> unit
  (** [set_mode_setter me] uses occurences of [me] to set, if possible,
      the current surface's application mode and the value of {!mode}. *)

  val mode_flip : 'a event -> mode event
  (** [mode_flip e] is an event which occur whenever [e] does
      with the opposite mode of {!mode}'s value. *)

  val pp_mode : Format.formatter -> mode -> unit
  (** [pp_mode ppf m] prints an unspecified representation of [m] on [ppf]. *)

  (** {1:surfaces Surface specification}

      See also {{!Useri_jsoo.Surface.sspec}surface specification information}
      inf the [`Jsoo] backend. *)

  (** The type for OpenGL surface specification. *)
  module Gl : sig

    (** {1 OpenGL surface} *)

    type colors = [ `RGBA_8888 | `RGB_565 ]
    (** The type for color buffers specification. *)

    type depth = [ `D_24 | `D_16 ]
    (** The type for depth buffer specification. *)

    type stencil = [ `S_8 ]
    (** The type for stencil buffer specification. *)

    type t =
      { accelerated : bool option;
        multisample : int option;
        doublebuffer : bool;
        stereo : bool;
        srgb : bool;
        colors : colors;
        depth : depth option;
        stencil : stencil option;
        version : int * int; }
    (** The type for OpenGL surface specifications.

        The default values mentionned below are those of {!default}.
        {ul
        {- [accelerated], use [Some false] for software renderer, [Some true]
            to require hardware renderer, [None] to allow either. Defaults to
            [None]}
        {- [multisample], use [Some count] for enabling a multisample buffer
            with [count] samples. Defaults to [Some 8].}
        {- [doublebuffer], use a double buffered surface. Defaults to [true].}
        {- [stereo], use stereo buffers. Defaults to [false].}
        {- [srgb], request an sRGB framebuffer. Defaults to [true].}
        {- [colors], specify the color buffers. Defaults to [`RGBA_8888].}
        {- [depth], specify the depth buffer. Defaults to [Some `D_24].}
        {- [stencil], specify the stencil buffer. Defaults to [None].}
        {- [version], specify the GL version.}} *)

    val default : t
    (** [default] is the default OpenGL surface specification. See {!t}. *)
  end

  type handle = Useri_base.Surface.handle
  (** The type for backend dependent surface handles. *)

  type kind = [ `Gl of Gl.t | `Other ]
  (** The type for surface kinds. *)

  type t
  (** The type for surface specification. *)

  val create : ?hidpi:bool -> ?pos:p2 -> ?size:size2 -> ?kind:kind ->
    ?handle:handle -> ?mode:mode -> unit -> t
  (** [create hidpi pos size kind anchor mode  ()] is a surface specification
       with:
      {ul
      {- [hidpi], if [true] (default) tries to get a high-dpi surface.}
      {- [pos], a {e hint} to position the surface by using [pos] as
         the position of the top-left corner of the surface in its
         backend dependent display container.  The coordinates of the
         top-left corner of the display container is [(0,0)] and
         coordinate increase in from top to bottom and left to
         right.}
      {- [size], the size of the surface in logical pixels. Consult
         the individual backend documentation for the default value.}
      {- [kind] is the kind of surface.}
      {- [handle] is a backend dependent surface handle. In certain
         backends allows to interface [Useri] with pre-existing surfaces.}
      {- [mode] is the the surface mode.}}

      {b Note.} Currently the [size] and [pos] arguments are
      specified The actual raster size of the surface is in the
      {!raster_size} signal. Note that future versions of this library
      will switch to physical dimensions (lack of backend support is
      the problem at the moment).  *)

  (** {1:surface Application Surface}

      See also information about {{!Useri_jsoo.Surface.propsupdate}surface
      properties updates} in the [`Jsoo] backend.

      {b Warning.} These values and functions are defined
      only after {!App.init} was called. *)

  val pos : p2 signal
  (** [pos] is the position of the top-left corner of the surface
      in the surface's backend dependent display container. *)

  val raster_size : size2 signal
  (** [raster_size] is the application's surface underlying raster
      size. *)

  val size : size2 signal
  (** [size] is the application's surface logical size. *)

  val update : unit -> unit
  (** [update ()] updates the rendering surface. This has
      to be called for your drawing commands to be taken into
      account. *)

  val handle : unit -> Useri_base.Surface.handle
  (** [handle ()] is the application's surface backend dependent handle. *)

  (** {1:refreshing Refreshing surfaces}

      These events are for coordinating rendering in an energy
      efficient manner. See {!rendercoord} for more information. *)

  val refresh : float event
  (** [refresh] occurs whenever the surface needs to be redrawn with
      the number of seconds since its last occurence or, for the
      first occurence, the number of seconds since the application
      start.

      The exact occurence frequency is unspecified but generally it
      should not exceed the {e hinted} frequency of {!refresh_hz}
      hertz.

      More precisely the following occurences are guaranteed:
      {ul
      {- Some time {e after} the {!size} signal changes value (including
         the first time during {!App.init}).}
      {- Some time {e after} an occurence of the event set by
         {!set_refresher}.}
      {- Some time {e after} a call to {!request_refresh}.}
      {- Some time {e after} a {!Time.count} or {!Time.unit}
         signal is created or updates.}}
      [Useri] guarantees that this event doesn't occur simultaneously
      with any of the events or signal it exposes. *)

  val request_refresh : unit -> unit
  (** [request_refresh ()] has the effect of making {!refresh}
      occur some time after it was called. This function call
      is cheap and can be abused (can be called in a React
      step).

      {b Warning.} This function may be removed from the API in
      the future. *)

  val set_refresher : 'a event -> unit
  (** [set_refresher r] uses sets [r] to be used to generate
      an occurence of {!refresh} some time after each occurence of [r]. *)

  val steady_refresh : until:'a event -> unit
  (** [steady_refresh until] makes {!refresh} occur at the hinted
      frequency of {!refresh_hz} hertz until [until] occurs. *)

  val refresh_hz : int signal
  (** [refresh_hz] is the maximal {e hinted} frequency in hertz for {!refresh}
      occurences. The initial value is [60]. *)

  val set_refresh_hz : int -> unit
  (** [set_refresh_hz] sets the value of {!refresh_hz}. *)
end

(** User mouse.

    Mouse events are only reported whenever the mouse is
    in the application surface. Coordinates are in surface noralized
    coordinates with [(0,0)] corresponding to the bottom left corner
    and [(1,1)] to the top right corner. *)
module Mouse : sig

  (** {1 Mouse position} *)

  val pos : p2 signal
  (** [pos] is the current mouse position. *)

  val dpos : v2 event
  (** [dpos] occurs on mouse moves with current mouse position minus
      the previous position. *)

  (** {1 Mouse buttons} *)

  val left : bool signal
  (** [left] is [true] whenever the left mouse button is down. *)

  val left_down : p2 event
  (** [left_down] has an occurence whenever the left mouse button goes down. *)

  val left_up : p2 event
  (** [left_up] has an occurence whenever the left mouse button goes up. *)

  val middle : bool signal
  (** [middle] is [true] whenever the middle mouse button is down. *)

  val middle_down : p2 event
  (** [middle_down] has an occurence whenever the middle mouse button goes
      down. *)

  val middle_up : p2 event
  (** [middle_up] has an occurence whenever the middle mouse button goes
      up. *)

  val right : bool signal
  (** [right] is [true] whenever the right mouse button is down. *)

  val right_up : p2 event
  (** [right_up] has an occurence whenever the right mouse button goes up. *)

  val right_down : p2 event
  (** [right_down] has an occurence whenever the right mouse button goes
      down. *)
end

(** User keyboard.

    These events and signals should only be used for treating keys as
    actuators. For getting textual input use the support in the
    {!Text} module.

    For the [`Jsoo] backend consult the important
    {{!Useri_jsoo.Key.get}information} about key handling.

    {!Useri_jsoo.Key} has important information about key handling
    in the [`Jsoo] backend.

    {{!repeat}Note} about the absence of key repeat events. *)
module Key : sig

  (** {1 Key identifiers} *)

  type id =
    [ `Alt of [ `Left | `Right ]
    | `Arrow of [ `Up | `Down | `Left | `Right ]
    | `Backspace
    | `Ctrl of [ `Left | `Right ]
    | `Digit of int
    | `End
    | `Enter
    | `Escape
    | `Function of int
    | `Home
    | `Meta of [ `Left | `Right ]
    | `Page of [ `Up | `Down ]
    | `Return
    | `Shift of [ `Left | `Right ]
    | `Space
    | `Tab
    | `Uchar of int
    | `Unknown of int ]
  (** The type for key identifiers.

      A key identifier corresponds to a physical key on a given
      keyboard. It is {b not related} to the textual character that will
      be inserted by depressing that key. Use {!Text} events for getting
      textual data inserted by a user. *)

  val uchar : char -> [> `Uchar of int ]
  (** [uchar c] is a key identifier from [c]. *)

  val pp_id : Format.formatter -> id -> unit
  (** [pp_id ppf id] prints an unspecified representation of [id] on
      [ppf]. *)

  (** {1 Key events and signals}

      Some of the signals below have caveats to consider in very
      improbable corner cases, see note on {{!semantics}semantics}.  *)

  val any_down : id event
  (** [any_down] occurs whenever a key goes down. *)

  val any_up : id event
  (** [any_up] occurs whenever a key goes up. *)

  val any_holds : bool signal
  (** [any_holds] is [true] whenever any key is down. *)

  val down : id -> unit event
  (** [down id] occurs whenever the key [id] goes down. *)

  val up : id -> unit event
  (** [up id] occurs whenever the key [id] goes up. *)

  val holds : id -> bool signal
  (** [holds id] is [true] whenever the key [id] is down. *)

  (** {1 Key modifiers signals} *)

  val alt : bool signal
  (** [alt] is [true] whenever an alt key is down. Equivalent to:
      {[S.Bool.(holds (`Alt `Left) || holds (`Right `Right))]} *)

  val ctrl : bool signal
  (** [ctrl] is [true] whenever a ctrl key is down. Equivalent to:
      {[S.Bool.(holds (`Ctrl `Left) || holds (`Ctrl `Right))]} *)

  val meta : bool signal
  (** [meta] is [true] whenever a meta key is down. Equivalent to:
      {[S.Bool.(holds (`Meta `Left) || holds (`Meta `Right))]} *)

  val shift : bool signal
  (** [shift] is [true] whenever a shift key is down. Equivalent to:
      {[S.Bool.(holds (`Shift `Left) || holds (`Shift `Right))]} *)

  (** {1:semantics Semantic subtleties}

      If keys are hold by the user during initialisation of [Useri],
      {!any_holds} and {!holds} may initially wrongly be [false]
      until corresponding keys are released.

      {!down}, {!up} and {!holds} may not be accurate the first time
      they are created inside a {!React} update cycle in which they
      should occur or update themselves. Note that this is quite
      unlikely, the only case where this can occur is if these
      functions are called as a dependency of the [Useri.Key.any_*]
      events or signals.

      {1:repeat Key repeat events}

      [Useri] doesn't expose key repeat events. There are two main use
      cases for key repeat. First during text input, this is handled
      by {!Text} events. Second, for controlling changes to a variable
      over time (e.g. scrolling with the keyboard). In the latter case
      it is better to use a function over a {!Surface.stopwatch}
      signal until the key is up. *)
end

(** User textual input and clipboard.

    {b Warning} This API is currently unsupported in the `Jsoo backend. *)
module Text : sig

  (** {1 Textual input}

      To use keyboard keys as actuators use the {!Key} module. *)

  val set_input_enabled : bool signal -> unit
  (** [set_input_enabled enabled] uses the signal [enabled] to determine
      whether {!input} and {!editing} events are enabled. Initially set
      to [S.const false]. *)

  val input : string event
  (** [input] occurs whenever text is input by the user and
      the signal given to {!set_input_enabled} is [true]. *)

  val editing : (string * int * int) event
  (** [editing] may occur with [(text, cpos, sel_len)] before {!input}
      whenever text is being composited for example if multiple key
      strokes are needed in order to input a character. [text] is the
      text being composed, [cpos] is the cursor position in the text
      and [sel_len] the selection length (if any). Only occurs if
      the signal given to {!set_input_enabled} is [true]. *)

  (** {1 Clipboard} *)

  val clipboard : string signal
  (** [clipboard] is the clipboard's textual content (if any). This
      signal is defined by changes in the application's environement
      and occurences of the event given to {!set_clipboard_setter}. *)

  val set_clipboard_setter : string event -> unit
  (** [set_clipboard_setter setter] on occurences of [setter] sets the
      clipboard's textual content. Initially set to [E.never]. Note
      that [Some ""] is the same as

      {b Warning.} [setter] should not depend on {!clipboard}. *)
end

(** User drag and drop.

    For the [`Jsoo] backend consult the
    {{!Useri_jsoo.Drop.limits}limitations}. *)
module Drop : sig

  (** {1 Files} *)

  type file = Useri_base.Drop.file
  (** The type for dropped files. *)

  (** Dropped files *)
  module File : sig

    (** {1 File} *)

    type t = file
    (** The type for dropped files. *)

    val path : file -> string
    (** [path f] is [f]'s file system path. *)

    val prepare : file ->
      (file -> [`Ok of unit | `Error of string] -> unit) -> unit
    (** [prepare f k] prepares files [f] for reading, calling [k f r] whenever
        [f] is ready to be read from (e.g. with {!Pervasives.open_in}).

        This is mainly used to hide the upload of the file in
        [js_of_ocaml]'s {{!Sys_js}pseudo file system} (e.g. on the
        [`Tsdl] backend it will immediately call [k]). If you need more
        control over the upload process you should access the JavaScript file
        object directly with {!Useri_jsoo.Drop.File.to_js}. *)

end

  val file : file event
  (** [file] occurs whenever a file is dropped on the application.

      An occurence corresponds either to a file or to a directory.
      Multiple files or directories can be dropped together and each
      of those will result in one occurence on {!file} (which means
      that you cannot detect that a set of files where dropped
      together). *)
end

(** Human factors. *)
module Human : sig

  (** {1 System latency feelings}

      These values are from
      {{:http://www.nngroup.com/articles/response-times-3-important-limits/}
      here}. *)

  val noticed : Time.span
  (** [noticed] is [0.1]s, the time span after which the user will
      notice a delay and feel that the system is not reacting
      instantaneously. *)

  val interrupted : Time.span
  (** [interrupted] is [1.]s, the time span after which the user will
      feel interrupted and feedback from the system is needed. *)

  val left : Time.span
  (** [left] is [10.]s, the time span after which the user will
      switch to another task and feedback indicating when the system
      expects to respond is needed. *)

  val feel : unit -> [ `Interacting | `Interrupted | `Left ] signal
  (** [feel ()] is a signal that varies according to user latency
      constants:
      {ul
      {- \[[user_feel ()]\]{_t} [= `Interacting] if
         [t < User.interrupted].}
      {- \[[user_feel ()]\]{_t} [= `Interrupted] if
         [t < User.left].}
      {- \[[user_feel ()]\]{_t} [= `Left] if [t >= User.left].}} *)
  (** {1 Touch target and finger sizes}

      These values are from
      {{:http://msdn.microsoft.com/en-us/library/windows/apps/hh465415.aspx#touch_targets}here}.
*)

  val touch_target_size : float
  (** [touch_target_size] is [9.]mm, the recommended touch target size in
      millimiters. *)

  val touch_target_size_min : float
  (** [touch_size_min] is [7.]mm, the minimal touch target size in
      millimeters. *)

  val touch_target_pad : float
  (** [touch_target_pad] is [2.]mm, the minimum padding size in
      millimeters between two touch targets. *)

  val average_finger_width : float
  (** [average_finger_width] is [11.]mm, the average {e adult} finger width. *)

end

(** Application  *)
module App : sig

  (** {1:environment Environment} *)

  val env : string -> default:'a -> (string -> 'a) -> 'a
  (** [env var ~default parse] lookups [var] in the environment, parses
      it with [parse] and returns the result. If [parse] raises or
      if [var] is not in the environment [default] is returned.

      Data lookup depends on the {{!backend}backend}:
      {ul
      {- [`Tsdl] uses {!Sys.getenv}.}
      {- [`Jsoo] lookups the query string of [window.location]
         for the first matching [var=value] pair.}} *)

  val prefs_path : org:string -> app:string ->
  [`Ok of string | `Error of string ]
  (** [TODO] this should used the app name automatically.
      Unique to user and app. *)

  (** {1:userquit User requested quit} *)

  val quit : unit event
  (** [quit] occurs whenever the user requested to quit. The meaning
      depends on the {{!backend}backend}:
      {ul
      {- [`Tsdl], this is only a hint, e.g. the last window
         was closed or a platform dependent way of quitting
         applications was invoked}
      {- [`Jsoo], the browser window is closing and it's your
         last chance to peform something}}. *)

  (** {1:lifecycle Init, run and release} *)

  val init : ?name:string -> ?surface:Surface.t -> unit ->
  [ `Ok of unit | `Error of string ]
  (** [init name surface] initialises an application named [name] (default
      derived from the executable name) with surface [surface] (defaults
      to a default {!Surface.create}.) *)

  val run_step : unit -> Time.span
  (** [run_step ()] gather as much user input as possible and returns
      the maximal timespan after which {!run_step} should be called again. *)

  val run : ?until:'a event -> unit -> unit
  (** [run until] depends on the {{!backend_scheme}backend scheme}:
      {ul
      {- [`Sync] invokes {!run_step} repeatedly and blocks until
         the first occurence of [until] (defaults to {!quit}).
         After {!run} returned it can be called again.}
      {- [`Async] returns immediately, [until] is irrelevant.}} *)

  val release : ?sinks:bool -> unit -> unit
  (** [release sinks ()] does the following:
      {ol
      {- Makes the {!stop} event occur.}
      {- If [sinks] is [true] (default), calls {!release_sinks}.}
      {- Reclaims other resources}}

      After a {!release} it should be possible to {!init} again. *)

  val start : unit event
  (** [start] occurs the first time either {!run_step} or {!run} is
      called. *)

  val stop : unit event
  (** [stop] occurs when {!release} starts. *)

  (** {1 Event and signal sinks} *)

  val sink_event : 'a event -> unit
  (** [sink_event e] keeps a reference on [e] until the app {!exit}s. *)

  val sink_signal : 'a signal -> unit
  (** [sink_signal s] keeps a reference on [s] until the app {!exit}s. *)

  val release_sinks : unit -> unit
  (** Stops and release sink references. In the [`Jsoo] {{!backend}backend}
      stops are {{!React.strongstop}strong}. *)

  (** {1 Launch context} *)

  type launch_context = [ `Browser | `Gui | `Terminal ]

  val launch_context : launch_context
  (** [launch_context] is the mechanism that started the program. *)

  val pp_launch_context : Format.formatter -> launch_context -> unit
  (** [pp_launch_context ppf c] prints an unspecified representation of
      [c] on [ppf]. *)

  (** {1 Platform and backend} *)

  val platform : string
  (** [platform] is the name of the platform you are running on.

      {b Warning.} Do not expect different backend to report the same
      platform with the same string. *)

  type backend = [ `Tsdl | `Jsoo | `Other of string ]
  (** The type for [Useri]'s backends. *)

  val backend : backend
  (** [backend] is [Useri]'s current backend. *)

  val pp_backend : Format.formatter -> backend -> unit
  (** [pp_backend ppf b] prints an unspecified representation of
      [c] on [ppf]. *)

  val set_backend_logger : ([`Warning | `Error ] -> string -> unit) -> unit
  (** [set_backend_logger log] sets [log] to be called by backends
      whenever non-fatal internal warnings and errors are generated.
      The default function outputs and flushes to stderr. *)

  type backend_scheme = [ `Sync | `Async ]
  (** The type for backend scheme.
      {ul
      {- [`Sync], the backend is synchronous the client of the library
         is in charge of running the event loop by using {!run_step}
         or {!run}.}
      {- [`Async], the backend is asynchronous, there's an inversion
         of control, the call to {!run} won't block.}} *)

  val backend_scheme : [ `Sync | `Async ]
  (** [backend_scheme] is the [Useri]'s current backend's scheme. *)

  val pp_backend_scheme : Format.formatter -> backend_scheme -> unit
  (** [pp_backend_scheme ppf bs] prints and unspecified representation of
      [bs] on [ppf]. *)

  (** {1 CPU count} *)

  type cpu_count = [ `Known of int | `Unknown ]
  (** The type for CPU counts. *)

  val cpu_count : cpu_count
  (** [cpu_count] is the number of CPU available. *)

  val pp_cpu_count : Format.formatter -> cpu_count -> unit
  (** [pp_cpu_count ppf c] prints an unspecified representation of [c]
      on [ppf]. *)
end

(**

   {1 Minimal example}

   This minimal example can be used on both synchronous
   and asynchronous backends.
{[

]}

   {1:cooperate Integration with cooperative concurency}

    A good way of managing side-effects at the boundaries of your
    functional reactive system is to use a cooperative concurency
    library and convert event occurences and signal changes to
    yielding futures/threads (to avoid the problem of forbidden
    recursive primitive feedback) and convert futures/threads to a
    primitive event with a single occurence.

    You will need however need to cooperate with [Useri]'s event
    loop and give it a high priority as the ability to interact
    should {b never} take over a long running computation.

    The following code shows how to do that with Lwt and Fut
{[
]}

  {1:rendercoord Input, animation and rendering coordination}

  The {!Time} and {!Surface} modules provide support for coordinating
  input, animation and rendering. We can distinguish two patterns for
  rendering:
  {ul
  {- Rendering as an effectful event or signal. This is more likely to be
     done in simple cases whenever you are not using cooperative concurrency.
     In this case rendering is simply performed in a
     {{!React.steps}React update step}
     simultanous with the {!Surface.refresh} event, in this step you can
     sample signals needed for rendering (as long as they {e do not}
     depend on {!Surface.refresh}).}
  {- Rendering as a task, in more complex rendering scenarios and
     especially with {{!cooperate}cooperative concurency}. In this
     case {!Surface.refresh} occurences simply generate a task that is
     run outside React's update step, this means that the signals
     needed for rendering can be safely sampled using {!React.S.value}
     for rendering data.}}
  In both cases rendering is coordinated by the {!Surface.refresh}
  event. It is important to use this event for the following reasons:
  {ul
  {- It occurs at important times in the life cycle of the application.
     For example whenever the application surface is first shown to the user
     or immediately after the application surface
     {!Surface.raster_size} changes.}
  {- If steady refreshes or timing signals are requested to perform animation
     it allows them to occur in an energy efficient way, avoiding overdraw.
     It will also gracefully cope with processing load by dropping animation
     frames if the rendering system cannot follow.}}
  There are various {e non-exclusive} ways of generating occurences of
  {!Surface.refresh}:
  {ul
  {- Steady refresh. If for a given time span until an event occurs
     (which may be {!E.never}) you need steady refreshing,
     use {!Surface.steady_refresh}.}
  {- Animation timing signals. {!Surface.refresh} occurences are generated
     immediately after the following signals are created and updated:
     {ul
     {- {!Time.unit}, for animations with a known time span, returns
        a signal increasing from [0.] to [1.] during that time span.}
     {- {!Time.count}, for animations with an unknown time span, returns
        a signal monotically increasing from [0.] to the time until a specified
        event occurs.}}}
  {- If there are a few events after which you know you need to redraw,
     {!E.select} them and register the resulting event with
     {!Surface.set_refresher}.}} *)

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
