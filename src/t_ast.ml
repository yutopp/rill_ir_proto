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
   | Module of {nodes: t list}
   | FuncDecl of {name: Id_string.t; params: Id_string.t list; body: t}
   | Stmts of {stmts: t list}
   | Let of {name: Id_string.t; expr: t}
   | Return of t
   | BinOp of {op: Id_string.t; lhs: t; rhs: t} (* TODO: change to Call *)
   | IfExpr of {cond: t; then_c: t; else_c: t}
   | Num of int
   | Bool of bool
   | Var of Id_string.t
[@@deriving show]
