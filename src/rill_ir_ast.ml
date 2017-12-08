module Type = Rill_ir_type
module Id_string = Rill_ir_id_string

(* Rill Core Ast *)
type 'loc t = {kind: 'loc kind_t; ty: Type.t; loc: 'loc}

 and 'loc kind_t =
   | Module of {nodes: 'loc t list}
   | FuncDecl of {name: Id_string.t; params: Id_string.t list; body: 'loc t}
   | Stmts of {stmts: 'loc t list}
   | Let of {name: Id_string.t; expr: 'loc t}
   | Return of 'loc t
   | BinOp of {op: Id_string.t; lhs: 'loc t; rhs: 'loc t} (* TODO: change to Call *)
   | IfExpr of {cond: 'loc t; then_c: 'loc t; else_c: 'loc t}
   | Num of int
   | Bool of bool
   | Var of Id_string.t
[@@deriving show]
