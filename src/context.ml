(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  mutable errors: Error.t list
}

exception Escape

let empty () =
  {
    errors = []
  }

let add_error ctx error =
  ctx.errors <- error :: ctx.errors

let has_errors ctx =
  List.length ctx.errors != 0

let get_errors ctx =
  ctx.errors |> List.rev

let escape_with_error ctx err loc =
  add_error ctx (err, loc);
  raise Escape
