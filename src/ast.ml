(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  kind: kind;
  loc:  Loc.t;
}
 and kind =
   | Module of t list
   | SpecModule of t
   | DefFunc of {id: t; params: t list; ret_spec: t; body: t}
   | DeclParam of {id: t; ty_spec: t}
   | TypeSpec of t
   | ExprIf of {cond: t; then_c: t; else_c: t option}
   | ExprBlock of t
   | ExprBinOp of {op: string; lhs: t; rhs: t}
   | ExprCall of {func: t; args: t list}
   | Var of t
   | Id of string
   | LitString of string
   | LitInt of {signed: bool; bits: int; value: int}
   | LitUnit
[@@deriving show]

let string_of_id t =
  match t with
  | {kind = Id s} -> s
  | _             -> failwith "not an id node"
