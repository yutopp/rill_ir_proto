(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

exception Escape

let error_with_exn ctx err loc () =
  Context.add_error ctx (err, loc);
  raise Escape

let rec find_type env tree =
  Type.Builtin.i1

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
     let id_s = Ast.string_of_id id in
     Env.add_venv id_s (Type.fresh ()) env

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
             | res              -> Some (res |> fst)
             | exception Escape -> None
            )
     in
     (T_ast.{kind = Module new_stmts; ty; loc}, env)

  | Ast.{kind = DefFunc {id; params; body}; loc} ->
     let id_s = Ast.string_of_id id in

     let pre_func_ty =
       Env.find_venv id_s env
       |> Option.default_delayed (error_with_exn ctx (Error.Not_found id_s) id.Ast.loc)
     in

     let (new_params_rev, env) =
       List.fold_left (fun (px, e) p ->
                       let (p', e') = analyze ctx e p in
                       (p' :: px, e'))
                      ([], env) params
     in
     let new_params = new_params_rev |> List.rev in

     let param_tys = new_params |> List.map (fun p -> p.T_ast.ty) in
     let func_ty = Type.Function (param_tys, Type.Builtin.unit) in

     let ty = func_ty in
     (* TODO: unify *)

     let (new_body, _) = analyze ctx env body in

     let ret_ty = Type.Builtin.never in
     let ret_body = T_ast.{kind = Return new_body; ty = ret_ty; loc} in

     (T_ast.{kind = FuncDecl {name = id_s; params = new_params; body = ret_body}; ty; loc}, env)

  | Ast.{kind = DeclParam {id; ty_spec}; loc} ->
     let ty = find_type env ty_spec in

     let id_s = Ast.string_of_id id in
     let env = Env.add_venv (Ast.string_of_id id) (Type.fresh ()) env in

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

  | Ast.{kind = LitInt {value; bits; signed}; loc} ->
     let ty = Type.Primitive (Int {bits; signed}) in
     (T_ast.{kind = Num value; ty; loc}, env)

  | Ast.{kind = Var id; loc} ->
     let id_s = Ast.string_of_id id in

     let ty =
       Env.find_venv id_s env
       |> Option.default_delayed (error_with_exn ctx (Error.Not_found id_s) id.Ast.loc)
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
