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
open GRAPHICS
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

let move_y0 = 20. *. fontHeight +. margin_top +. margin_bottom

let clearText c (x,y) color text =
  let width = float_of_int ( String.length text ) *. fontWidth in
  Canvas.setFillColor c color ;
  Canvas.fillRect c ~pos:(x, y -. fontHeight) ~size: ( width , 1.5 *. fontHeight );
  ()

let fonts = [|
  "fixed";
  "Arial" ;
|]

let draw_box s (x,y) width ?bg ?(fg=foreground) text =
  (*  Printf.eprintf "draw_box %0.1f %0.1f\n%!" x y; *)
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


let rec draw_block ?(main= -1) ?(pos=0) s (x,y) b =

  (*  Printf.eprintf "draw_block %0.1f %0.1f\n%!" x y; *)
  let dx0, dy0 = draw_box s (x,y) field_box_width b.block_class.class_name
      ~bg:class_color
  in

  let len = Array.length b.block_fields in
  let rec iter ~pos i ~dx y =
    if i < len then
      let (label, value) = b.block_fields.(i) in
      let (dxl, dy) =
        draw_box s (x,y) field_box_width label
          ?bg:(if pos = main then
                 begin
                   s.pos_y <- y ;
                   Some Color.red
                 end else None) in
      let (dxr, dy, pos) =
        match value with
        | Value v ->
            let (dxr, dy) =
              draw_box s (x +. dxl, y) value_box_width v
            in
            (dxr, dy, pos+1)
        | Pointer _ -> assert false
        | Block bb ->
            let (dxr, dy) =
              draw_box s (x +. dxl, y) value_box_width
                (match bb.block_uid with
                 | None -> assert false
                 | Some uid -> Printf.sprintf "ptr%d" uid)
            in
            (dxr, dy, pos+1)
        | InlineBlock bb ->
            let indent = 30. in
            let (dx, dy2, pos) =
              draw_block ~main ~pos s (x +. indent, y +. dy) bb in
            (dx +. indent, dy +. dy2, pos)
        | _ -> (0., dy, pos+1)
      in
      let dx1 = dxl +. dxr in
      let dx = if dx1 > dx then dx1 else dx in
      iter ~pos (i+1) ~dx (y +. dy)
    else
      (dx,y, pos)
  in
  let (_dx, y1, pos) = iter ~pos 0 ~dx:dx0 (y +. dy0) in
  (0., y1 -. y, pos)


let rec get_block s ?(pos=0) main b =

  let len = Array.length b.block_fields in
  let rec iter ~pos i =
    if i < len then
      let (_label, value) = b.block_fields.(i) in
      match value with
      | Value _ ->
          if pos = main then
            `NotABlock
          else
            let pos = pos + 1 in
            iter ~pos (i+1)
      | Pointer _ -> assert false
        | Block bb ->
            if pos = main then
              `Block bb
            else
              let pos = pos + 1 in
            iter ~pos (i+1)
        | InlineBlock bb ->
            begin
              match get_block s ~pos main bb with
              | `NotFound pos ->
                  iter ~pos (i+1)
              | x -> x
            end
        | _ ->
            let pos = pos + 1 in
            iter ~pos (i+1)
    else
      `NotFound pos
  in
  iter ~pos 0

let count = ref 0
let draw s =
  incr count;
  Printf.eprintf "draw at %.1f %.1f (w = %f h = %f)\n%!" s.x0 s.y0
    s.canvas_width s.canvas_height;
  let c = s.canvas in
  Canvas.setFillColor c background; (*(Color.bof_rgb (5* !count) !count !count); *)
  Canvas.fillRect c ~pos:(-100.,-100.) ~size:(s.canvas_width +. 200.,
                                              s.canvas_height +. 200.) ;
  Canvas.setFont c fonts.(0) ~size:fontHeight
    ~slant:Font.Roman ~weight:Font.regular;
  let p0 = (s.x0, s.y0) in
  begin
    match s.path with
      [] -> assert false
    | (_root, b, pos) :: _ ->
        let (_x,_y,size) = draw_block ~main:pos s p0 b  in
        s.size <- size
  end;
  (*
  Canvas.setStrokeColor c Color.blue;
  Canvas.setLineWidth c 10.0;
  Canvas.clearPath c;
  Canvas.moveTo c (50.0, 50.0);
  Canvas.lineTo c (750.0, 50.0);
  Canvas.lineTo c (750.0, 450.0);
  Canvas.lineTo c (50.0, 450.0);
  Canvas.closePath c;
  Canvas.stroke c;

  Canvas.moveTo c (150.0, 350.0);
  Canvas.stroke c;
*)
  (*
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

  let path = [ "<ROOT>", tree, 0 ] in
  let c = Canvas.createOnscreen ~title:"Data Vizualizer"
      ~autocommit:false
      ~pos:(300, 200) ~size:(canvas_width, canvas_height) () in

  let s = {
    x0 = start_x0 ;
    y0 = start_y0 ;
    canvas_width = float_of_int canvas_width ;
    canvas_height = float_of_int canvas_height ;
    path ;
    size = 0;
    pos_y = start_y0 ;
    canvas = c ;
  } in
  Canvas.show s.canvas ;
  Printf.eprintf "setFont\n%!";
  Canvas.setFont c fonts.(0) ~size:fontHeight
    ~slant:Font.Roman ~weight:Font.bold;

  Canvas.must_redraw s.canvas;


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
*)

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
              | KeySpacebar
              | KeyReturn
              | KeyQ ->
                  Backend.stop ()
              | KeyUpArrow ->
                  if flags.flag_control then begin
                    s.y0 <- s.y0 +. move_y0 ;
                    Canvas.must_redraw s.canvas
                  end else begin
                    match s.path with
                    | [] -> assert false
                    | (root, b, pos) :: path ->
                        if pos > 0 then begin
                          s.path <- (root, b, pos-1) :: path;
                          if s.pos_y < start_y0 then
                            s.y0 <- s.y0 +. move_y0;
                          Canvas.must_redraw s.canvas
                        end
                  end
              | KeyDownArrow ->
                  if flags.flag_control then begin
                    s.y0 <- s.y0 -. move_y0 ;
                    Canvas.must_redraw s.canvas
                  end else begin
                    match s.path with
                    | [] -> assert false
                    | (root, b, pos) :: path ->
                        if pos+1 < s.size then begin
                          s.path <- (root, b, pos+1) :: path;
                          if s.pos_y >
                             s.canvas_height -. start_y0 then
                            s.y0 <- s.y0 -. move_y0;
                          Canvas.must_redraw s.canvas
                        end
                  end
              | KeyLeftArrow ->
                  begin
                    Printf.eprintf "left\n%!";
                    match s.path with
                    | _ :: (( _ :: _ ) as path ) ->
                        Printf.eprintf "LEFT\n%!";
                        s.path <- path ;
                        s.y0 <- start_y0 ;
                        Canvas.must_redraw s.canvas
                    | _ -> ()
                  end
              | KeyRightArrow ->
                  begin
                    match s.path with
                    | [] -> assert false
                    | (root, b, pos) :: _ ->
                        match get_block s pos b with
                        | `Block bb ->
                            s.path <- (root, bb, 0) :: s.path;
                            s.y0 <- start_y0 ;
                            Canvas.must_redraw s.canvas
                        | _ -> ()
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
                   data = { Event.position = _pos; button = _ }} ->
              Printf.eprintf "EVENT button_down\n%!";
            ) Event.button_down;

  add_event (fun { Event.canvas = _; timestamp = _;
                   data = (x,y)} ->
              Printf.eprintf "EVENT redraw\n%!";
              s.canvas_width <- float_of_int x;
              s.canvas_height <- float_of_int y;
              draw s ;
            ) Event.redraw;

  add_event (fun { Event.canvas = _; timestamp = _;
                   data = _ } ->
              Printf.eprintf "EVENT mouse_move\n%!";
              ()
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
