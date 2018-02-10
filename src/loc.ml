(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = pos option
 and pos = {
   pos_begin_cnum : int;
   pos_begin_lnum : int;
   pos_begin_bol  : int;
   pos_end_cnum   : int;
   pos_end_lnum   : int;
   pos_end_bol    : int;
 }
[@@deriving show]

let show_message loc =
  match loc with
  | Some pos ->
     Printf.sprintf "%d:%d" pos.pos_begin_lnum pos.pos_begin_bol
  | None ->
     "<unknown>"
