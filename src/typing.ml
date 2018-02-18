(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

exception Type_mismatch of {expect: Type.t; actual: Type.t}
exception Variable_not_found of string

let type_check ~expect ~actual ctx loc =
  if expect <> actual then
    let e = Type_mismatch {expect; actual} in
    Context.escape_with_error ctx e loc

let rec find_type env tree =
  Type.Builtin.i1

let rec pre_collect_types pre_env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left pre_collect_types pre_env stmts

  | _ ->
     pre_env

let rec collect_toplevel_types env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left collect_toplevel_types env stmts

  | _ ->
     env

let rec collect_toplevel_functions env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left collect_toplevel_functions env stmts

  | Ast.{kind = DefFunc {id; params}} ->
     let param_tys =
       params
       |> List.map (fun p ->
                    match p with
                    | Ast.{kind = DeclParam {ty_spec}; loc} ->
                       let ty = find_type env ty_spec in
                       ty
                    | _ ->
                       failwith "[ICE] not DeclParam"
                   )
     in
     let ret_ty = Type.Builtin.unit in
     let func_ty = Type.Function (param_tys, ret_ty) in

     let id_s = Ast.string_of_id id in
     Env.add_venv id_s func_ty env

  | _ ->
     failwith "TOP"

let rec analyze ctx env tree =
  match tree with
  | Ast.{kind = Module stmts; loc} ->
     let ty = Type.Builtin.i1 in
     let new_stmts =
       stmts
       |> List.filter_map
            (fun s ->
             match analyze ctx env s with
             | res                      -> Some (res |> fst)
             | exception Context.Escape -> None
            )
     in
     (T_ast.{kind = Module new_stmts; ty; loc}, env)

  | Ast.{kind = DefFunc {id; params; body}; loc} ->
     let id_s = Ast.string_of_id id in

     let ty = match Env.find_venv id_s env with
       | Some v -> v
       | None   -> Context.escape_with_error ctx (Variable_not_found id_s) id.Ast.loc
     in
     let (_param_tys, ret_ty) = Type.as_func ty in

     let (new_params_rev, env) =
       List.fold_left (fun (px, e) p ->
                       let (p', e') = analyze ctx e p in
                       (p' :: px, e'))
                      ([], env) params
     in
     let new_params = new_params_rev |> List.rev in

     let (new_body, _) = analyze ctx env body in
     let () = type_check ~expect:ret_ty ~actual:new_body.T_ast.ty ctx body.Ast.loc in

     let ret_expr_ty = Type.Builtin.never in
     let ret_body = T_ast.{kind = Return new_body; ty = ret_expr_ty; loc} in

     (T_ast.{kind = FuncDecl {name = id_s; params = new_params; body = ret_body}; ty; loc}, env)

  | Ast.{kind = DeclParam {id; ty_spec}; loc} ->
     let ty = find_type env ty_spec in

     let id_s = Ast.string_of_id id in
     let env = Env.add_venv (Ast.string_of_id id) ty env in

     (T_ast.{kind = DeclParam id_s; ty; loc}, env)

  | Ast.{kind = ExprIf {cond; then_c; else_c}; loc} ->
     let (new_cond, env') = analyze ctx env cond in
     let new_then_c = then_c |> analyze ctx  env' |> fst in
     let new_else_c = else_c |> Option.map (analyze ctx  env' %> fst) in
     let ty = match new_else_c with
       | Some else_c_node ->
          (* TODO: unify *)
          new_then_c.T_ast.ty
       | None ->
          (* TODO: check else clause *)
          new_then_c.T_ast.ty
     in
     (T_ast.{kind = IfExpr {cond = new_cond; then_c = new_then_c; else_c = new_else_c}; ty; loc}, env)

  | Ast.{kind = ExprBlock e; loc} ->
     let new_e = analyze ctx env e |> fst in
     (* TODO: add scope *)
     (new_e, env)

  | Ast.{kind = ExprCall {func; args}; loc} ->
     let new_func = analyze ctx env func |> fst in
     let new_args = args |> List.map (analyze ctx env %> fst) in

     let func_ty = new_func.T_ast.ty in
     let (arg_tys, ret_ty) = Type.as_func func_ty in

     (* TODO: unify *)
     (T_ast.{kind = ExprCall {func = new_func; args = new_args}; ty = ret_ty; loc}, env)

  | Ast.{kind = LitInt {value; bits; signed}; loc} ->
     let ty = Type.Primitive (Int {bits; signed}) in
     (T_ast.{kind = Num value; ty; loc}, env)

  | Ast.{kind = LitUnit; loc} ->
     let ty = Type.Primitive (Unit) in
     (T_ast.{kind = Unit; ty; loc}, env)

  | Ast.{kind = Var id; loc} ->
     let id_s = Ast.string_of_id id in

     let ty =
       match Env.find_venv id_s env with
       | Some v -> v
       | None   -> Context.escape_with_error ctx (Variable_not_found id_s) id.Ast.loc
     in

     (T_ast.{kind = Var id_s; ty; loc}, env)

  | _ ->
     failwith (Printf.sprintf "ANALYZER: %s" (Ast.show tree))

let add_primitive_types env =
  let env = Env.add_tenv "i32" Type.Builtin.i32 env in
  env

let rec generate ctx env tree =
  let env = add_primitive_types env in
  let env = collect_toplevel_types env tree in
  let env = collect_toplevel_functions env tree in
  analyze ctx env tree