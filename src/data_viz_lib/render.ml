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

open Types

open OcamlCanvas.V1 (* why not OCamlCanvas ? *)

let canvas_width = 1500
let canvas_height = 1000
let start_x0 = 200.
let start_y0 = 200.

let fontHeight = 14.0
let fontWidth = 10.
let field_box_width = 250.
let value_box_width = 300.
let margin_left = 5.
let margin_right = 5.
let margin_top = 5.
let margin_bottom = 5.

let background = Color.white
let foreground = Color.black
let class_color = Color.of_rgb 200 200 200

let move_y0 = fontHeight +. margin_top +. margin_bottom

let clearText c (x,y) color text =
  let width = float_of_int ( String.length text ) *. fontWidth in
  Canvas.setFillColor c color ;
  Canvas.fillRect c ~pos:(x, y -. fontHeight) ~size: ( width , 1.5 *. fontHeight );
  ()

let fonts = [|
  "fixed";
  "Liberation Sans" ;
|]

let draw_box s (x,y) width ?bg ?(fg=foreground) text =
  Printf.eprintf "draw_box %0.1f %0.1f\n%!" x y;
  let c = s.canvas in
  let pos = (x,y) in
  let size = (width, fontHeight +. margin_top +. margin_bottom) in
  begin
    match bg with
    | None -> ()
    | Some bg ->
        Canvas.setFillColor c bg;
        Canvas.fillRect c ~pos ~size
  end;
  Canvas.setStrokeColor c fg;
  Canvas.strokeRect c ~pos ~size ;
  Canvas.strokeText c text
    (x +. margin_left, y +. margin_top +. fontHeight);
  ignore (text, fg);
  size


let draw_block s (x,y) b =

  Printf.eprintf "draw_block %0.1f %0.1f\n%!" x y;
  let _dx0, dy0 = draw_box s (x,y) field_box_width b.block_class.class_name
      ~bg:class_color
  in

  let len = Array.length b.block_fields in
  let rec iter i y =
    if i < len then
      let (label, value) = b.block_fields.(i) in
      let (dx, dy) = draw_box s (x,y) field_box_width label in
      let (_dx, dy) =
        match value with
        | Value v ->
            draw_box s (x +. dx, y) value_box_width v
        | _ -> (0., dy)
      in
      iter (i+1) (y +. dy)
  in
  iter 0 (y +. dy0);
  ()

let draw s =
  Printf.eprintf "draw at %.1f %.1f\n%!" s.x0 s.y0;
  let c = s.canvas in
  Canvas.setFillColor c background;
  Canvas.fillRect c ~pos:(0.,0.) ~size:(s.canvas_width, s.canvas_height) ;
  Canvas.setFont c fonts.(0) ~size:fontHeight
    ~slant:Font.Roman ~weight:Font.bold;
  let p0 = (s.x0, s.y0) in
  begin
    match s.path with
      [] -> assert false
    | (_root, b) :: _ ->
        draw_block s p0 b ;
  end;
  Canvas.commit c;
  (*
  Canvas.setStrokeColor c Color.blue;
  Canvas.setLineWidth c 10.0;
  Canvas.moveTo c (50.0, 50.0);
  Canvas.lineTo c (750.0, 50.0);
  Canvas.lineTo c (750.0, 450.0);
  Canvas.lineTo c (50.0, 450.0);
  Canvas.closePath c;
  Canvas.stroke c;

  Canvas.clearPath c;
  Canvas.moveTo c (150.0, 350.0);
  Canvas.bezierCurveTo c ~cp1:(350.0, -100.0)
    ~cp2:(450.0, 600.0) ~p:(650.0, 150.0);
  Canvas.stroke c;

  Canvas.setFont c "Liberation Sans" ~size:54.0
    ~slant:Font.Roman ~weight:Font.bold;

  Canvas.save c;
  Canvas.setFillColor c (Color.of_rgb 0 192 0);
  Canvas.translate c (400.0, 250.0);
  Canvas.rotate c (-0.1 *. Const.pi);
  Canvas.translate c (-400.0, -250.0);
  Canvas.fillText c "Hello world !" (200.0, 250.0);
  Canvas.restore c;
*)
    ()

let events = ref []
let add_event f e =
  events := ( React.E.map f e ) :: !events

let render tree =

  Backend.init ();

  let path = [ "<ROOT>", tree ] in
  let c = Canvas.createOnscreen ~title:"Data Vizualizer"
      ~autocommit:false
      ~pos:(300, 200) ~size:(canvas_width, canvas_height) () in

  let s = {
    x0 = start_x0 ;
    y0 = start_y0 ;
    canvas_width = float_of_int canvas_width ;
    canvas_height = float_of_int canvas_height ;
    path ;
    canvas = c ;
  } in
  Canvas.show s.canvas ;
  Printf.eprintf "setFont\n%!";
  Canvas.setFont c fonts.(0) ~size:fontHeight
    ~slant:Font.Roman ~weight:Font.bold;

  draw s;


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

  let draw_pos (x,y) =

    let p1 = (10.0, 40.0) in
    let p2 = (200.0, 40.0) in
    Canvas.setStrokeColor c Color.black;
    Canvas.setFillColor c background;

    clearText c p1 background "     ";
    Canvas.strokeText c (string_of_int x) p1;
    clearText c p2 background "     ";
    Canvas.strokeText c (string_of_int y) p2;
  in

  add_event (fun { Event.canvas = _; timestamp = _; data = () } ->
      Printf.eprintf "EVENT close\n%!";
      Backend.stop ()
    ) Event.close
  ;
  add_event (fun { Event.canvas = _; timestamp = _;
                   data = { Event.key; char = _; flags} } ->
              Printf.eprintf "EVENT key_down\n%!";
              match key with
              | KeyEscape
              | KeyQ ->
                  Backend.stop ()
              | KeyUpArrow ->
                  if flags.flag_control then begin
                    s.y0 <- s.y0 +. move_y0 ;
                    draw s
                  end
              | KeyDownArrow ->
                  if flags.flag_control then begin
                    s.y0 <- s.y0 -. move_y0 ;
                    draw s
                  end
              | _ -> ()
              (* Arrow pad
                 | KeyUpArrow
                 | KeyLeftArrow
                 | KeyDownArrow
                 | KeyRightArrow
              *)
            ) Event.key_down
  ;

  add_event (fun { Event.canvas = _; timestamp = _;
                   data = { Event.position = pos; button = _ }} ->
              Printf.eprintf "EVENT button_down\n%!";
              draw_pos pos ;

            ) Event.button_down;

  add_event (fun { Event.canvas = _; timestamp = _;
                   data = (x,y)} ->

              Printf.eprintf "EVENT rezise\n%!";
              s.canvas_width <- float_of_int x;
              s.canvas_height <- float_of_int y;

              draw s ;

            ) Event.resize;

  add_event (fun { Event.canvas = _; timestamp = _;
                   data = _ } ->

              (*  Canvas.save c;
                  Canvas.setFillColor c (Color.of_rgb 0 192 0);
                  Canvas.translate c (400.0, 250.0);
                  Canvas.rotate c (-0.1 *. Const.pi);
                  Canvas.translate c (-400.0, -250.0);
                  Canvas.fillText c "Hello world !" (200.0, 250.0);
                  Canvas.restore c;
              *)
              ()

  (*
Canvas.setFillColor c Color.red;
        Canvas.clearPath c;
        Canvas.arc c ~center:(float_of_int x, float_of_int y)
          ~radius:5.0 ~theta1:0.0 ~theta2:(2.0 *. Const.pi) ~ccw:false;
        Canvas.fill c ~nonzero:false
*)
            ) Event.mouse_move
  ;

  add_event (fun { Event.canvas = _; timestamp = _ ; _ } ->
      Printf.eprintf "EVENT focus_in\n%!";
    ) Event.focus_in ;
  ;

  add_event (fun { Event.canvas = _; timestamp = _ ; _ } ->
      Printf.eprintf "EVENT focus_out\n%!";
    ) Event.focus_out ;
  ;


  let frames = ref 0L in

  add_event (fun { Event.canvas = _; timestamp = _ ; _ } ->
      frames := Int64.add !frames Int64.one
    ) Event.frame
  ;

  Backend.run (fun () ->
      Printf.printf "Displayed %Ld frames. Goodbye !\n" !frames
    )
