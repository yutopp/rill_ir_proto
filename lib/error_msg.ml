(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let string_of_exn e =
  match e with
  | Typing.Type_mismatch {expect; actual} ->
     Printf.sprintf "Type mismatch: expect=%s, actual=%s" (Type.show expect) (Type.show actual)
  | _ ->
     Printf.sprintf "[ICE] Unexpected: %s" (Batteries.dump e)

let to_msg err =
  let (e, loc) = err in
  Printf.sprintf "%s: %s" (Loc.show_message loc) (string_of_exn e)
