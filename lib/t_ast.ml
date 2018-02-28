(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  kind: kind;
  ty: Type.t;
  loc: Loc.t
}
 and kind =
   | Module of t list
   | FuncDecl of {name: Id_string.t; params: t list; body: t}
   | ExprSeq of t list
   | Let of {name: Id_string.t; expr: t}
   | Return of t
   | IfExpr of {cond: t; then_c: t; else_c: t option}
   | ExprCall of {func: t; args: t list}
   | Num of int
   | Unit
   | Bool of bool
   | Id of Id_string.t
   | DeclParam of Id_string.t
[@@deriving show]
