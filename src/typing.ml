(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let rec generate env tree =
  match tree with
  | Ast.{kind = Module stmts; loc} ->
     let i1_imm = Type.{base = Concrete Bool; mutability = Immutable} in
     (T_ast.{kind = Num 0; ty = i1_imm; loc}, env)
  | _ ->
     failwith ""
