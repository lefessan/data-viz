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

type block_class = {
  class_name : string ;
}

type block = {
  block_class : block_class ;

  mutable block_fields : ( string * value ) array ;
  mutable block_uid : int option ;
  mutable block_path : string option ;
  mutable block_preds : block list ;
}

and value =
  | Value of string
  | Block of block
  | InlineBlock of block
  | Pointer of int
  | Array of value array

type state = {
  canvas : OcamlCanvas.V1.Canvas.t ;
  mutable x0 : float ;
  mutable y0 : float ;
  mutable canvas_width : float ;
  mutable canvas_height : float ;
  mutable path : (string * block) list ;
}
