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

open Ez_file.V1

open Types

let load_file file =

  let lines = EzFile.read_lines file in

  let nlines = Array.length lines in

  let classes = Hashtbl.create 111 in
  let find_class name =
    match Hashtbl.find classes name with
    | cl -> cl
    | exception Not_found ->
        let cl = { class_name = name } in
        Hashtbl.add classes name cl;
        cl
  in

  let get_line i =
    let line = lines.(i) in
    let i = i + 1 in
    (* Printf.eprintf "%05d: %s\n%!" i line; *)
    line, i
  in

  let rec iter0 i =
    let line, i = get_line i in
    iter1 i line

  and iter1 i line =
    let key, arg = EzString.cut_at line ' ' in
    match key with
    | "MEMORY_BLOCK" ->
        let n, i = iter_block (Some (int_of_string arg)) i in
        Block n, i
    | "INLINE_BLOCK" ->
        let n, i = iter_block None i in
        InlineBlock n, i
    | "ENUM" -> Value arg, i
    | "STRING" -> Value arg, i
    | "INT" -> Value arg, i
    | "ABSTRACT" -> Value arg, i
    | "ALREADY_SEEN" -> Pointer (int_of_string arg), i
    | "NULL" -> Value "NULL", i
    | "ARRAY" ->
        let t, i = iter_array [] i in
        Array t, i
    | _ -> assert false

  and iter_block uid i =
    let line, i = get_line i in
    let key, arg = EzString.cut_at line ' ' in
    match key with
    | "BEGIN_STRUCT" ->
        let cl = find_class arg in
        iter_fields uid cl [] i
    | _ -> assert false

  and iter_fields uid cl fields i =
    let line, i = get_line i in
    let key, arg = EzString.cut_at line ' ' in
    match key with
    | "END_STRUCT" ->
        let n = {
          block_class = cl;
          block_fields = List.rev fields |> Array.of_list ;
          block_uid = uid ;
          block_path = None;
          block_preds = [];
        } in
        n, i
    | "FIELD" ->
        let field, arg = EzString.cut_at arg ' ' in
        let v, i = iter1 i arg in
        iter_fields uid cl ( (field,v) :: fields ) i
    | _ -> assert false

  and iter_array vals i =
    let line, i = get_line i in
    let key, arg = EzString.cut_at line ' ' in
    match key with
    | "END_ARRAY" ->
        let t = List.rev vals |> Array.of_list in
        t, i
    | "NEW_ITEM" ->
        let v, i = iter1 i arg in
        iter_array ( v :: vals ) i
    | _ -> assert false

  in
  let block, i = iter0 0 in
  assert (i = nlines);

  match block with
  | Block block -> block
  | _ -> assert false
