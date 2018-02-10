(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = (kind * Loc.t)
 and kind =
  | Not_found of string

let string_of_kind kind =
  match kind with
  | Not_found name ->
     Printf.sprintf "not found: %s" name

let show_message error =
  let (kind, loc) = error in
  Printf.sprintf "%s: %s" (Loc.show_message loc) (string_of_kind kind)

let dump error =
  Printf.printf "%s\n" (show_message error)
