(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Type_ref = struct
  (* updated only when the building phase *)
  type t = {
    mutable state: state;
  }
  and state =
    | Complete of kind
    | Prototype
  and kind =
    | Primitive of primitive
    | Function of t list * t
  and primitive =
    | Never
    | Unit
    | Int of {bits: int; signed: bool}
  [@@deriving show]

  let empty =
    {
      state = Prototype;
    }

  module Builtin = struct
    let never =
      {
        state = Complete (Primitive Never)
      }

    let unit =
      {
        state = Complete (Primitive Unit)
      }

    let int ~bits ~signed =
      {
        state = Complete (Primitive (Int {bits; signed}))
      }

    let func ~params ~ret =
      {
        state = Complete (Function (params, ret))
      }
  end
end

module Ast = struct
  type t = {
    kind: kind;
    ty: Type_ref.t;
    loc: Loc.t;
  }
  and kind =
    | ExprFunc of {param_specs: (string * Loc.t) list; body: t}
    | ExprSeq of t list
    | ExprLet of {name: string; expr: t}
    | ExprReturn of t option
    | ExprIf of {cond_e: t; then_e: t; else_e_opt: t option}
    | ExprCall of {func: t; args: t list}
    | LitNum of int
    | LitUnit
    | Var of string
  [@@deriving show]
end

module Func_ref = struct
  (* updated only when the building phase *)
  type t = {
    name: string;
    mutable state: state;
    mutable param_specs: param_spec list;
    mutable ret_ty: Type_ref.t;
    mutable body: Ast.t option;
  }
  and state =
    | Complete
    | Prototype
  and param_spec = {
    param_name: string;
    param_ty: Type_ref.t;
  }
  [@@deriving show]

  let create name =
    {
      name;
      state = Prototype;
      param_specs = [];
      ret_ty = Type_ref.empty;
      body = None;
    }
end

module Var_ref = struct
  type t = {
    mutable kind: kind;
  }
  and kind =
    | Undefined
    | Expr of Ast.t

  let create name =
    {
      kind = Undefined
    }
end

type t = {
  tenv: (string, Type_ref.t) Hashtbl.t;
  fenv: (string, Func_ref.t) Hashtbl.t;
  venv: (string, Var_ref.t) Hashtbl.t;
  (*venv: (string, type_ref) Hashtbl.t;*)
}

let create () =
  {
    tenv = Hashtbl.create 0;
    fenv = Hashtbl.create 0;
    venv = Hashtbl.create 0;
  }

let build_var_decl m name =
  let v = Var_ref.create name in
  Hashtbl.add m.venv name v

let create_param_spec ~loc name =
  (name, loc)

let create_expr_func ~ty ~loc ?(param_specs=[]) body =
  Ast.{kind = ExprFunc {param_specs; body}; ty; loc}

let update_var m name v =
  let v_ref = Hashtbl.find m.venv name in
  v_ref.Var_ref.kind <- Var_ref.Expr v

let create_expr_seq ~ty ~loc exprs =
  Ast.{kind = ExprSeq exprs; ty; loc}

let create_expr_let ~ty ~loc name expr =
  Ast.{kind = ExprLet {name; expr}; ty; loc}

let create_expr_return ~loc expr =
  let ty = Type_ref.Builtin.never in
  Ast.{kind = ExprReturn expr; ty; loc}

let create_expr_if ~ty ~loc cond_e then_e else_e_opt =
  Ast.{kind = ExprIf {cond_e; then_e; else_e_opt}; ty; loc}

let create_expr_call ~ty ~loc func args =
  Ast.{kind = ExprCall {func; args}; ty; loc}

let create_lit_num ~ty ~loc v =
  Ast.{kind = LitNum v; ty; loc}

let create_lit_unit ~loc =
  let ty = Type_ref.Builtin.unit in
  Ast.{kind = LitUnit; ty; loc}

let create_var ~ty ~loc s =
  Ast.{kind = Var s; ty; loc}
