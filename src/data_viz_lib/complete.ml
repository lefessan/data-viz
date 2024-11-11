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

let complete block =

  let negative_uids = ref 0 in
  let new_uid () =
    decr negative_uids ;
    !negative_uids
  in

  let blocks = Hashtbl.create 1111 in

  let rec iter_value value =
    match value with
    | Pointer _
    | Value _
      -> ()

    | Block block -> iter_block block
    | InlineBlock block -> iter_block block

    | Array values ->
        Array.iter iter_value values

  and iter_block block =
    let uid = match block.block_uid with
      | None ->
          let uid = new_uid () in
          block.block_uid <- Some uid ;
          uid
      | Some uid -> uid
    in
    if not @@ Hashtbl.mem blocks uid then
      let () = () in
      Hashtbl.add blocks uid block;
      Array.iter (fun (_, v) -> iter_value v) block.block_fields

  in
  iter_block block ;

  let rec fix_block b =
    Array.iteri (fun i (label, v) ->
        match v with
        | Pointer uid ->
            begin
              match Hashtbl.find blocks uid with
              | exception Not_found -> assert false
              | bb ->
                  bb.block_preds <- b :: bb.block_preds ;
                  b.block_fields.(i) <- (label, Block bb)
            end
        | Array vs -> fix_array b vs
        | _ -> ()
      ) b.block_fields

  and fix_array b t =
    Array.iteri (fun i v ->
        match v with
        | Pointer uid ->
            begin
              match Hashtbl.find blocks uid with
              | exception Not_found -> assert false
              | bb ->
                    bb.block_preds <- b :: bb.block_preds ;
                    t.(i) <- Block bb
            end
        | Array vs -> fix_array b vs
        | _ -> ()
      ) t

  in
  Hashtbl.iter (fun _ b ->
      fix_block b
    ) blocks;

  block
