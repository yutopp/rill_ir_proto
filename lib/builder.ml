(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

let rec build_top_level_pre m node =
  match node with
  | T_ast.{kind = Module nodes; ty; loc} ->
     List.iter (build_top_level_pre m) nodes
  | T_ast.{kind = FuncDecl {name; params; body}; ty; loc} ->
     Mir.build_var_decl m name;
  | _ ->
     ()

let rec to_type_ref m ty =
  match ty with
  | Type.Primitive Never ->
     Mir.Type_ref.Builtin.never
  | Type.Primitive Unit ->
     Mir.Type_ref.Builtin.unit
  | Type.Primitive (Int {bits; signed}) ->
     Mir.Type_ref.Builtin.int ~bits ~signed
  | Type.Function (params, ret) ->
     let params' = List.map (to_type_ref m) params in
     let ret' = to_type_ref m ret in
     Mir.Type_ref.Builtin.func ~params:params' ~ret:ret'
  | _ ->
     failwith "[ICE] unexpected type"

let to_param_spec node =
  match node with
  | T_ast.{kind = DeclParam name; ty; loc} ->
     Mir.create_param_spec ~loc name
  | _ ->
     failwith "[ICE] unexpected node in param"

let rec build_top_level m node =
  match node with
  | T_ast.{kind = Module nodes; ty; loc} ->
     List.iter (build_top_level m) nodes
  | T_ast.{kind = FuncDecl {name; params; body}; ty; loc} ->
     let param_specs = params |> List.map to_param_spec in

     let ty' = to_type_ref m ty in
     let body' = create_expr m body in
     let f = Mir.create_expr_func ~ty:ty' ~loc ~param_specs body' in

     Mir.update_var m name f
  | _ ->
     failwith ""

and create_expr m node =
  match node with
  | T_ast.{kind = ExprSeq exprs; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_expr_seq ~ty:ty' ~loc (List.map (create_expr m) exprs)
  | T_ast.{kind = Let {name; expr}; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_expr_let ~ty:ty' ~loc name (create_expr m expr)
  | T_ast.{kind = Return expr; ty; loc} ->
     Mir.create_expr_return ~loc (create_expr m expr |> Option.some)
  | T_ast.{kind = IfExpr {cond; then_c; else_c}; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_expr_if ~ty:ty'
                        ~loc
                        (create_expr m cond)
                        (create_expr m then_c)
                        (else_c |> Option.map (create_expr m))
  | T_ast.{kind = ExprCall {func; args}; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_expr_call ~ty:ty'
                          ~loc
                          (create_expr m func)
                          (args |> List.map (create_expr m))
  | T_ast.{kind = Num v; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_lit_num ~ty:ty' ~loc v
  | T_ast.{kind = Bool b; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_lit_num ~ty:ty' ~loc (if b then 1 else 0)
  | T_ast.{kind = Unit; ty; loc} ->
     Mir.create_lit_unit ~loc
  | T_ast.{kind = Id s; ty; loc} ->
     let ty' = to_type_ref m ty in
     Mir.create_var ~ty:ty' ~loc s
  | _ ->
     failwith "[ICE] unexpected node in expr"

let build node =
  let m = Mir.create () in
  build_top_level_pre m node;
  build_top_level m node;
  m
