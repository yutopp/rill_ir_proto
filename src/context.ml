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

let empty () =
  {
    errors = []
  }

let add_error ctx error =
  ctx.errors <- error :: ctx.errors

let has_errors ctx =
  List.length ctx.errors != 0

let dump_errors ctx =
  ctx.errors
  |> List.rev
  |> List.iter Error.dump
