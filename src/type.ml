(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type primitive =
  | Never
  | Unit
  | Int of {bits: int; signed: bool}
[@@deriving show]

type t =
  | Var of int
  | Primitive of primitive
  | Function of t list * t
[@@deriving show]

module Builtin = struct
  let never =
    Primitive (Never)

  let unit =
    Primitive (Unit)

  let i1 =
    Primitive (Int {bits = 1; signed = true})

  let i32 =
    Primitive (Int {bits = 32; signed = true})
end

let as_func ty =
  match ty with
  | Function (params, ret) -> (params, ret)
  | _ -> failwith "Not function type"
