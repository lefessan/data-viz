(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* A new event Event.redraw is used: all drawing must be done in this
   event handler. If you need to draw in another event, just call
   Canvas.must_redraw after changing the global state, so that a
   Event.redraw is signaled just after.

  Limitations: * timestamps in events are not implemented *)

let _TODO () = if false then assert false

module V1 = struct


  type color_rgba = float * float * float * float
  type color_id = int * int * int * int

  type color = color_id * color_rgba

  type font_slant = Roman | Italic | Oblique
  type font_weight = Normal | Bold

  type gtk = {
    drawing_area : GMisc.drawing_area ;
    mutable context : Cairo.context option ;
    mutable current_color : color_id option ;
    mutable current_font : (string * int * font_slant * font_weight) option;
  }

  type canvas =
    {
      window : GWindow.window ;
      mutable stroke_color : color ;
      mutable fill_color : color ;
      mutable font : (string * int * font_slant * font_weight) option ;
      mutable gtk : gtk option ;
    }

  module Color = struct

    let of_rgba r g b a =
      let id = (r, g, b, a) in
      let rgba = (float_of_int r /. 256.,
                  float_of_int g /. 256.,
                  float_of_int b /. 256.,
                  float_of_int a /. 256.
                 ) in
      (id, rgba)

    let of_rgb r g b = of_rgba r g b 255

    let white = of_rgb 255 255 255
    let black = of_rgb 0 0 0
    let red = of_rgb 255 0 0
    let green = of_rgb 0 255 0
    let blue = of_rgb 0 0 255
    let yellow = of_rgb 255 255 0
  end

  module Font = struct
    type slant = font_slant = Roman | Italic | Oblique
    type weight = font_weight
    let bold = Bold
    let regular = Normal
  end

  module Backend = struct

    let exitfun = ref (fun () -> ())

    let init () =
      ignore(GMain.init());
      ()

    let stop () =
      !exitfun ();
      GMain.quit ()

    let run f =
      exitfun := f;
      GMain.main ()
  end


  module Event = struct

    type timestamp = Int64.t

    type 'a canvas_event = {
      canvas: canvas;
      timestamp: timestamp;
      data: 'a;
    }

    type position = int * int

    type size = int * int

    type key =
      (* Function *)
      | KeyEscape
      | KeyF1
      | KeyF2
      | KeyF3
      | KeyF4
      | KeyF5
      | KeyF6
      | KeyF7
      | KeyF8
      | KeyF9
      | KeyF10
      | KeyF11
      | KeyF12
      | KeyPrintScreen (* absent from Mac Keyboards *)
      | KeyScrollLock (* absent from Mac Keyboards *)
      | KeyPause (* absent from Mac Keyboards *)

      (* Alphanumeric, first row *)
      | KeyGraveTilde
      | Key1Exclamation
      | Key2At
      | Key3Number
      | Key4Dollar
      | Key5Percent
      | Key6Caret
      | Key7Ampersand
      | Key8Asterisk
      | Key9LParenthesis
      | Key0RParenthesis
      | KeyMinusUndersclre
      | KeyEqualPlus
      | KeyBackspace

      (* Alphanumeric, second row *)
      | KeyTab
      | KeyQ
      | KeyW
      | KeyE
      | KeyR
      | KeyT
      | KeyY
      | KeyU
      | KeyI
      | KeyO
      | KeyP
      | KeyLBracketCurly
      | KeyRBracketCurly
      | KeyBackslashPipe (* replaced by KeyNonUSNumberTilde on ISO KB *)

      (* Alphanumeric, third row *)
      | KeyCapsLock
      | KeyA
      | KeyS
      | KeyD
      | KeyF
      | KeyG
      | KeyH
      | KeyJ
      | KeyK
      | KeyL
      | KeySemicolonColon
      | KeyQuoteDoublequote
      | KeyNonUSNumberTilde (* extra key left of Return on ISO KB *)
      (* although generally mapped to KeyBackslashPipe *)
      | KeyReturn

      (* Alphanumeric, fourth row *)
      | KeyLShift
      | KeyNonUSBackslashPipe (* extra key right of LShift on ISO KB *)
      | KeyZ
      | KeyX
      | KeyC
      | KeyV
      | KeyB
      | KeyN
      | KeyM
      | KeyCommaLess
      | KeyPeriodGreater
      | KeySlashQuestion
      | KeyRShift

      (* Alphanumeric, fifth row *)
      | KeyLControl
      | KeyLMeta (* left Windows / Command key *)
      | KeyLAlt
      | KeySpacebar
      | KeyRAlt
      | KeyRMeta (* right Windows / Command key *)
      | KeyMenu
      | KeyRControl

      (* Control pad *)
      | KeyInsert (* replaced by a Fn key on Mac (with a different code) *)
      | KeyHome
      | KeyPageUp
      | KeyDeleteForward
      | KeyEend
      | KeyPageDown

      (* Arrow pad *)
      | KeyUpArrow
      | KeyLeftArrow
      | KeyDownArrow
      | KeyRightArrow

      (* Numeric pad *)
      | KeyPadNumlockClear (* on Mac, Clear replaces NumLock *)
      | KeyPadEquals (* on Mac keyboards only *)
      | KeyPadDivide
      | KeyPadMultiply
      | KeyPadMinus
      | KeyPad7Home
      | KeyPad8UpArrow
      | KeyPad9PageUp
      | KeyPadPlus
      | KeyPad4LeftArrow
      | KeyPad5
      | KeyPad6RightArrow
      | KeyPadComma (* specific to Brazilian keyboards *)
      | KeyPad1End
      | KeyPad2DownArrow
      | KeyPad3PageDown
      | KeyPad0Insert
      | KeyPadDecimalDelete
      | KeyPadEnter

      (* Extra function keys *)
      | KeyF13
      | KeyF14
      | KeyF15
      | KeyF16
      | KeyF17
      | KeyF18
      | KeyF19
      | KeyF20
      | KeyF21
      | KeyF22
      | KeyF23
      | KeyF24

      (* International & LANG keys *)
      | KeyInternational1 (* extra key left of RShift on JIS and Brazilian KB *)
      | KeyInternational2 (* Katakana/Hiragana key right of Space on JIS KB *)
      | KeyInternational3 (* extra key left of Backspace on JIS KB *)
      | KeyInternational4 (* Henkan key right of Space on JIS KB *)
      | KeyInternational5 (* Muhenkan key left of Space on JIS KB *)
      | KeyInternational6 (* Kanma (comma) key right of KP0 on JIS KB *)
      | KeyInternational7 (* Double-Byte/Single-Byte toggle key *)
      | KeyInternational8 (* Undefined *)
      | KeyInternational9 (* Undefined *)
      | KeyLang1 (* Hangul/English toggle key (Korean) *)
      | KeyLang2 (* Hanja conversion key (Korean) *)
      | KeyLang3 (* Katakana key (Japanese) *)
      | KeyLang4 (* Hiragana key (Japanese) *)
      | KeyLand5 (* Zenkaku/Hankaku key (Japanese) *)

      (* Extensions *)
      | KeyHelp
      | KeyMute
      | KeyVolumeUp
      | KeyVolumeDown

    type flags = {
      mutable flag_shift : bool;
      mutable flag_alt : bool;
      mutable flag_control : bool;
      mutable flag_meta : bool;
      mutable flag_capslock : bool;
      mutable flag_numlock : bool;
      mutable flag_dead : bool;
    }

    type key_data = {
      key: key;
      char: Uchar.t;
      flags: flags;
    }

    type button =
      | ButtonNone
      | ButtonLeft
      | ButtonMiddle
      | ButtonRight
      | ButtonWheelUp
      | ButtonWheelDown

    type button_data = {
      position: position;
      button: button;
    }

    type 'a ev = 'a React.event * (?step:React.step -> 'a -> unit)

    let frame, send_frame =
      (React.E.create () : unit canvas_event ev)
    let focus_in, send_focus_in =
      (React.E.create () : unit canvas_event ev)
    let focus_out, send_focus_out =
      (React.E.create () : unit canvas_event ev)
    let resize, send_resize =
      (React.E.create () : size canvas_event ev)
    let move, send_move =
      (React.E.create () : position canvas_event ev)
    let close, send_close =
      (React.E.create () : unit canvas_event ev)
    let key_down, send_key_down =
      (React.E.create () : key_data canvas_event ev)
    let key_up, send_key_up =
      (React.E.create () : key_data canvas_event ev)
    let button_down, send_button_down =
      (React.E.create () : button_data canvas_event ev)
    let button_up, send_button_up =
      (React.E.create () : button_data canvas_event ev)
    let mouse_move, send_mouse_move =
      (React.E.create () : position canvas_event ev)
    let redraw, send_redraw =
      (React.E.create () : size canvas_event ev)
    (* let backend_stop, send_backend_stop =
       (React.E.create () : backend_stop_event ev) *)

    let event_timestamp, set_event_timestamp =
      React.S.create 0L

  end

  module Canvas = struct
    type t = canvas

    let setFillColor canvas color =
      canvas.fill_color <- color
    let setStrokeColor canvas color =
      canvas.stroke_color <- color

    let get_context gtk =
      match gtk.context with
      | Some c -> c
      | None -> assert false

    let set_color gtk color =
      if
        match gtk.current_color with
        | None -> true
        | Some id -> id <> fst color
      then
        let id, (r,g,b,a) = color in
        gtk.current_color <- Some id ;
        let context = get_context gtk in
        Cairo.set_source_rgba context r g b a ;
        ()

    let get_gtk canvas =
      match canvas.gtk with
      | None -> assert false
      | Some gtk -> gtk

    let fillRect canvas ~pos:(x,y) ~size:(dx,dy) =
      let gtk = get_gtk canvas in
      set_color gtk canvas.fill_color ;
      let c = get_context gtk in
      Cairo.rectangle c x y ~w:dx ~h:dy ;
      Cairo.fill c

    let strokeRect canvas ~pos:(x,y) ~size:(dx,dy) =
      let gtk = get_gtk canvas in
      set_color gtk canvas.stroke_color ;
      let c = get_context gtk in
      Cairo.rectangle c x y ~w:dx ~h:dy ;
      Cairo.stroke c

    let set_font canvas =
      match canvas.font with
      | None -> ()
      | Some f ->
          let gtk = get_gtk canvas in
          if
            match gtk.current_font with
            | None -> true
            | Some f_old -> f_old <> f
          then
            let (font, size, slant, weight) = f in
            let context = get_context gtk in
            Cairo.select_font_face context font
              ~slant:(match slant with
                  | Roman -> Cairo.Upright
                  | Oblique -> Cairo.Oblique
                  | Italic -> Cairo.Italic)
              ~weight:(match weight with
                  | Normal -> Cairo.Normal
                  | Bold -> Cairo.Bold);
            Cairo.set_font_size context (float_of_int size);
            gtk.current_font <- Some f

    let strokeText canvas text (x,y) =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      set_color gtk canvas.stroke_color ;
      Cairo.move_to c x y;
      set_font canvas ;
      Cairo.show_text c text;
      Cairo.stroke c ;
      ()

    let setLineWidth canvas w =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      Cairo.set_line_width c w

    let clearPath canvas =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      Cairo.Path.clear c

    let closePath canvas =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      Cairo.Path.close c

    let moveTo canvas (x,y) =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      Cairo.move_to c x y

    let lineTo canvas (x,y) =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      Cairo.line_to c x y

    let stroke canvas =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      set_color gtk canvas.stroke_color ;
      Cairo.stroke c

    let fill canvas =
      let gtk = get_gtk canvas in
      let c = get_context gtk in
      set_color gtk canvas.fill_color ;
      Cairo.fill c

    let setFont canvas font ~size ~slant ~weight =
      canvas.font <- Some (font, int_of_float size, slant, weight)

    let clear_context gtk =
      gtk.current_color <- None ;
      gtk.current_font <- None ;
      gtk.context <- None

    let internal_commit canvas =
      match canvas.gtk with
      | None -> ()
      | Some gtk ->
          match gtk.context with
          | None -> ()
          | Some _context ->
              (* Cairo.Surface.finish (Cairo.get_target context) ;*)
              clear_context gtk;
              ()

    let commit _canvas = ()

    let createOnscreen
        ~title
        ~autocommit:_
        ~pos:_
        ~size:(width, height)
        ()
      =
      let window = GWindow.window ~title ~width ~height () in

      let c =
        { gtk = None ;
          window ;
          stroke_color = Color.black ;
          fill_color = Color.black ;
          font = None ;
        }
      in

      let close () =

        let timestamp = 0L in
        let e = { Event.canvas = c ; timestamp ; data = ()} in
        Event.set_event_timestamp e.timestamp; Event.send_focus_in e ;

        Backend.stop ()
      in

      ignore(window#connect#destroy ~callback:close);
      c

    let must_redraw c =
      match c.gtk with
      | None -> ()
      | Some gtk -> gtk.drawing_area#misc#queue_draw ()

    let show c =

      let drawing_area = GMisc.drawing_area ~packing:c.window#add () in
      c.window#show();
      drawing_area#event#add [ `ALL_EVENTS ];
      drawing_area#misc#set_sensitive true;
      let gtk = { drawing_area ;
                  context = None ;
                  current_color = None ;
                  current_font = None ;
                } in
      c.gtk <- Some gtk ;

      let draw cr =

        gtk.context <- Some cr;
        let allocation = drawing_area#misc#allocation in
        let w = allocation.Gtk.width in
        let h = allocation.Gtk.height in

        let timestamp = 0L in
        let e = { Event.canvas = c ; timestamp ; data = (w,h)} in
        Event.set_event_timestamp e.timestamp; Event.send_redraw e ;

        internal_commit c;
        true
      in
      ignore(drawing_area#misc#connect#draw ~callback:draw);

  (*
  val focus_in : unit canvas_event React.event
    val focus_out : unit canvas_event React.event
    val resize : size canvas_event React.event
    val move : position canvas_event React.event
    val close : unit canvas_event React.event
    val key_down : key_data canvas_event React.event
    val key_up : key_data canvas_event React.event
    val button_down : button_data canvas_event React.event
    val button_up : button_data canvas_event React.event
    val mouse_move : size canvas_event React.event
*)

      let on_key pressed (event : GdkEvent.Key.t) =
        let open Event in
        let keyval = GdkEvent.Key.keyval event in
        let state = GdkEvent.Key.state event in

        let key =
          let open GdkKeysyms in

          if keyval = _q || keyval = _Q then Event.KeyQ else
          if keyval = _Return then Event.KeyReturn else
          if keyval = _space then Event.KeySpacebar else
          if keyval = _Escape then Event.KeyEscape else
          if keyval = _Left then KeyLeftArrow else
          if keyval = _Right then KeyRightArrow else
          if keyval = _Up then KeyUpArrow else
          if keyval = _Down then KeyDownArrow else
            KeyF24
        in

        let char = Uchar.of_char 'q' in (* TODO *)
        let flags = {
          Event.flag_shift = false ; (* `SHIFT *)
          flag_capslock = false ;    (* `LOCK *)
          flag_control = false ;     (* `CONTROL *)
          flag_alt = false ;         (* `MOD1 *)
          flag_meta = false ;        (* `META *)
          flag_numlock = false ;
          flag_dead = false ;
        } in
        List.iter (fun m ->
            match m with
            | `SHIFT ->
                flags.flag_shift <- true
            | `LOCK ->
                flags.flag_capslock <- true
            | `CONTROL ->
                flags.flag_control <- true
            | `META ->
                flags.flag_meta <- true
            | `MOD1 ->
                flags.flag_alt <- true
            | `MOD2
            | `MOD3
            | `MOD4
            | `MOD5
            | `SUPER
            | `HYPER
            | `RELEASE
            | `BUTTON1
            | `BUTTON2
            | `BUTTON3
            | `BUTTON4
            | `BUTTON5 -> ()
          ) state;
        let timestamp = 0L in
        let e = { Event.canvas = c ; timestamp ;
                  data = { Event.key ; char ; flags }} in
        Event.set_event_timestamp e.timestamp;
        if pressed then
          Event.send_key_down e
        else
          Event.send_key_up e ;

        true
      in
      ignore (
        c.window#event#connect#key_press ~callback:(on_key true) );
      ignore (
        c.window#event#connect#key_release ~callback:(on_key false) );

      let on_focus enter _ =

        let data = () in
        let timestamp = 0L in
        let e = { Event.canvas = c ; timestamp ; data } in
        Event.set_event_timestamp e.timestamp;
        if enter then
          Event.send_focus_in e
        else
          Event.send_focus_out e ;
        true
      in
      ignore (
        gtk.drawing_area#event#connect#focus_in ~callback:(on_focus true) );
      ignore (
        gtk.drawing_area#event#connect#focus_out ~callback:(on_focus false) );

      let on_button pressed (event : GdkEvent.Button.t) =
        let open Event in

        let button = GdkEvent.Button.button event in
        let button = match button with
          | 0 -> ButtonNone
          | 1 -> ButtonLeft
          | 2 -> ButtonMiddle
          | 3 -> ButtonRight
            (*
          | ButtonWheelUp
          | ButtonWheelDown
*)
          | _ -> ButtonNone
        in
        let x = GdkEvent.Button.x event in
        let y = GdkEvent.Button.y event in
        let data = {
          position = (int_of_float x,int_of_float y);
          button ;
        } in

        let timestamp = 0L in
        let e = { Event.canvas = c ; timestamp ; data } in
        Event.set_event_timestamp e.timestamp;
        if pressed then
          Event.send_button_down e
        else
          Event.send_button_up e ;
        true
      in
      ignore (
        gtk.drawing_area#event#connect#button_press ~callback:(on_button true) );
      ignore (
        gtk.drawing_area#event#connect#button_release ~callback:(on_button false) );

  end

end
