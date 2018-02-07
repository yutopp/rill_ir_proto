(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open OUnit2
open Batteries

let () =
  let open Rill_ir in
  Printexc.record_backtrace true;

  let tree =
    let t = Syntax.make_ast_from_file "simple01.rir" in
    assert_equal (Result.is_ok t) true;
    Result.get t
  in
  let () = tree |> Ast.show |> Printf.printf "%s\n" in
  let env = Env.empty () in
  let (typed_tree, env) = Typing.generate env tree in
  let () = typed_tree |> T_ast.show |> Printf.printf "%s\n" in

  (**)
  let tree =
    let open T_ast in
    let loc = None in
    let mod_imm = Type.{base = Concrete Module; mutability = Immutable} in
    let unit_imm = Type.{base = Concrete Unit; mutability = Immutable} in
    let i1_imm = Type.{base = Concrete Bool; mutability = Immutable} in
    let i32_imm = Type.{base = Concrete Int32; mutability = Immutable} in
    let fun_imm = Type.{base = Concrete (Func ([i32_imm], i32_imm)); mutability = Immutable} in

    let if_expr =
      let exp_10_20 = {kind = BinOp {op = "+";
                                     lhs = {kind = Num 10; ty = i32_imm; loc};
                                     rhs = {kind = Num 20; ty = i32_imm; loc}};
                       ty = i32_imm;
                       loc} in
      let exp_30_c = {kind = BinOp {op = "+";
                                    lhs = exp_10_20;
                                    rhs = {kind = Var "c"; ty = i32_imm; loc}};
                      ty = i32_imm;
                      loc} in
      {kind = IfExpr {cond = {kind = Bool false; ty = i1_imm; loc};
                      then_c = exp_30_c;
                      else_c = {kind = Num 500; ty = i32_imm; loc}};
       ty = i32_imm;
       loc}
    in
    (*let ret = {
      kind = Return {kind = Num 0; ty = i32_imm; loc = 5};
      ty = unit_imm;
      loc = 5
    } in*)
    let ret = {
      kind = Return if_expr;
      ty = unit_imm;
      loc
    } in
    let stmts = {kind = Stmts {stmts = [
                                  ret;
                                ]}; ty = unit_imm; loc} in
    let func0 = {kind = FuncDecl {name = "f"; params = ["c"]; body = stmts}; ty = fun_imm; loc} in
    {kind = Module {nodes = [func0]}; ty = mod_imm; loc}
  in(*
  let tree =
    let open Ast in
    let mod_imm = Type.{base = Concrete Module; mutability = Immutable} in
    let unit_imm = Type.{base = Concrete Unit; mutability = Immutable} in
    let i32_imm = Type.{base = Concrete Int32; mutability = Immutable} in
    let fun_imm = Type.{base = Concrete (Func ([i32_imm], i32_imm)); mutability = Immutable} in
    let ret = {
      kind = Return {kind = Num 10; ty = i32_imm; loc = 5};
      ty = unit_imm;
      loc = 5
    } in
    let stmts = {kind = Stmts {stmts = [
                                  ret;
                                ]}; ty = unit_imm; loc = 5} in
    let func0 = {kind = FuncDecl {name = "f"; params = ["c"]; body = stmts}; ty = fun_imm; loc = 4} in
    {kind = Module {nodes = [func0]}; ty = mod_imm; loc = 1}
  in*)
  let () = tree |> T_ast.show |> Printf.printf "%s\n" in
  let env = Env.empty () in
  let k_form = tree |> K_normal.generate env in
  let () = k_form |> K_normal.show (Loc.pp) |> Printf.printf "%s\n" in

  let ir_ctx = Ir.make_context () in
  let rill_module = k_form |> Ir.generate ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "%s\n" in

  let rill_module = rill_module |> Ir.complete_pass ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "%s\n" in

  let rill_module = rill_module |> Ir.reduce_tmp_vars_pass ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "%s\n" in

  let rill_module = rill_module |> Ir.collect_stack_pass ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "%s\n" in

  let backend_ctx = Backend.make_context () in
  let llvm_m = rill_module |> Backend.generate backend_ctx in
  let () = llvm_m |> Backend.show |> Printf.printf "%s\n" in
  ()
