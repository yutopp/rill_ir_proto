type t = {
  kind: kind;
  ty: Type.t;
  loc: Loc.t
}
 and kind =
   | Module of {nodes: t list}
   | FuncDecl of {name: Id_string.t; params: t list; body: t}
   | Seq of {nodes: t list}
   | Let of {name: Id_string.t; expr: t}
   | Call of {name: Id_string.t; args: Id_string.t list}
   | Assign of {lhs: Id_string.t; rhs: Id_string.t}
   | Return of Id_string.t
   | IfStmt of {cond: Id_string.t; then_c: t; else_c: t}
   | Num of int
   | Unit
   | Var of Id_string.t
   | DeclParam of Id_string.t
   | Undef
[@@deriving show]

let insert_let k_form k =
  match k_form with
  | {kind = Var id} ->
     k id
  | {loc} ->
     let new_id = Id_string.flesh () in
     let let_stmt = {kind = Let {name = new_id; expr = k_form}; ty = Type.Builtin.unit; loc} in
     match k new_id with
     | {kind = Seq {nodes}; ty; loc} ->
        {kind = Seq {nodes = let_stmt :: nodes}; ty; loc}
     | {ty; loc} as node ->
        {kind = Seq {nodes = [let_stmt; node]}; ty; loc}

let rec generate env ast =
  match ast with
  | T_ast.{kind = Module nodes; ty; loc} ->
     {kind = Module {nodes = List.map (generate env) nodes}; ty; loc}

  | T_ast.{kind = FuncDecl {name; params; body}; ty; loc} ->
     let params' = List.map (generate env) params in
     let body' = generate env body in

     {kind = FuncDecl {name; params = params'; body = body'}; ty; loc}

  | T_ast.{kind = Let {name; expr}; ty; loc} ->
     let expr' = generate env expr in
     {kind = Let {name; expr = expr'}; ty; loc}

  | T_ast.{kind = Return e; ty; loc} ->
     let k = insert_let (generate env e) in
     k (fun e' ->
        {kind = Return e'; ty; loc})

  | T_ast.{kind = BinOp {op; lhs; rhs}; ty; loc} ->
     let k = insert_let (generate env lhs) in
     k (fun lhs' ->
        let k = insert_let (generate env rhs) in
        k (fun rhs' ->
           {kind = Call {name = op; args = [lhs'; rhs']}; ty; loc}))

  | T_ast.{kind = IfExpr {cond; then_c; else_c}; ty; loc} ->
     let unit_imm = Type.Builtin.unit in
     let k = insert_let {kind = Undef; ty; loc} in
     k (fun holder' ->
        let k = insert_let (generate env cond) in
        k (fun cond' ->
           let then_c_v =
             let k = insert_let (generate env then_c) in
             k (fun v' -> {kind = Assign {lhs = holder'; rhs = v'}; ty = unit_imm; loc})
           in
           let else_c_v =
             match else_c with
             | Some else_c_node ->
                let k = insert_let (generate env else_c_node) in
                k (fun v' -> {kind = Assign {lhs = holder'; rhs = v'}; ty = unit_imm; loc})
             | None ->
                {kind = Assign {lhs = holder'; rhs = "unit<TMP>"}; ty = unit_imm; loc}
           in
           let if_stmt = {kind = IfStmt {cond = cond'; then_c = then_c_v; else_c = else_c_v}; ty = unit_imm; loc} in
           let var = {kind = Var holder'; ty; loc} in
           {kind = Seq {nodes = [if_stmt; var]}; ty; loc}
          ))

  | T_ast.{kind = ExprCall {func = T_ast.{kind = Var f}; args}; ty; loc} ->
     let rec bind xs args =
       match args with
       | [] -> {kind= Call {name=f; args = List.rev xs}; ty; loc}
       | a :: args ->
          let k = insert_let (generate env a) in
          k (fun k -> bind (k :: xs) args)
     in
     bind [] args

  | T_ast.{kind = ExprCall {func; args}; ty; loc} ->
     failwith ""

  | T_ast.{kind = Var x; ty; loc} ->
     {kind = Var x; ty; loc}

  | T_ast.{kind = Num n; ty; loc} ->
     {kind = Num n; ty; loc}

  | T_ast.{kind = Unit; ty; loc} ->
     {kind = Unit; ty; loc}

  | T_ast.{kind = DeclParam id; ty; loc} ->
     {kind = DeclParam id; ty; loc}

  | _ ->
     failwith "[ICE] not supported node"
