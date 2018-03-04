type t = {
  kind: kind;
  ty: Mir.Type_ref.t;
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
   | ReturnVoid
   | IfStmt of {cond: Id_string.t; then_c: t; else_c: t}
   | Num of int
   | Unit
   | Var of Id_string.t
   | DeclParam of Id_string.t
   | ExprFunc of {params: string list; body: t} (* fix types of params *)
   | Undef
[@@deriving show]

let insert_let k_form k =
  match k_form with
  | {kind = Var id} ->
     k id
  | {loc} ->
     let new_id = Id_string.flesh () in
     let let_stmt = {kind = Let {name = new_id; expr = k_form}; ty = Mir.Type_ref.Builtin.unit; loc} in
     match k new_id with
     | {kind = Seq {nodes}; ty; loc} ->
        {kind = Seq {nodes = let_stmt :: nodes}; ty; loc}
     | {ty; loc} as node ->
        {kind = Seq {nodes = [let_stmt; node]}; ty; loc}

let rec generate env ast =
  match ast with
  | Mir.Ast.{kind = ExprFunc {param_specs; body}; ty; loc} ->
     let env' =
       List.fold_left (fun e param_spec ->
                       let (param_name, _) = param_spec in
                       e
                      ) env param_specs
     in
     let params' = List.map (fun (n, loc) -> n) param_specs in
     let body' = generate env' body in
     {kind = ExprFunc {params = params'; body = body'}; ty; loc}

  | Mir.Ast.{kind = ExprLet {name; expr}; ty; loc} ->
     let expr' = generate env expr in
     {kind = Let {name; expr = expr'}; ty; loc}

  | Mir.Ast.{kind = ExprSeq exprs; ty; loc} ->
     {kind = Seq {nodes = (List.map (generate env) exprs)}; ty; loc}

  | Mir.Ast.{kind = ExprReturn (Some e); ty; loc} ->
     let k = insert_let (generate env e) in
     k (fun e' ->
        {kind = Return e'; ty; loc})
  | Mir.Ast.{kind = ExprReturn None; ty; loc} ->
     {kind = ReturnVoid; ty; loc}
  | Mir.Ast.{kind = ExprIf {cond_e; then_e; else_e_opt}; ty; loc} ->
     let k = insert_let {kind = Undef; ty; loc} in (* result *)
     k (fun holder' ->
        let k = insert_let (generate env cond_e) in
        k (fun cond' ->
           let then_c_v =
             let k = insert_let (generate env then_e) in
             k (fun v' -> {kind = Assign {lhs = holder'; rhs = v'}; ty = Mir.Type_ref.Builtin.unit; loc})
           in
           let else_c_v =
             match else_e_opt with
             | Some else_e ->
                let k = insert_let (generate env else_e) in
                k (fun v' -> {kind = Assign {lhs = holder'; rhs = v'}; ty = Mir.Type_ref.Builtin.unit; loc})
             | None ->
                {kind = Assign {lhs = holder'; rhs = "unit<TMP>"}; ty = Mir.Type_ref.Builtin.unit; loc}
           in
           let if_stmt = {
             kind = IfStmt {cond = cond'; then_c = then_c_v; else_c = else_c_v};
             ty = Mir.Type_ref.Builtin.unit;
             loc
           } in
           let var = {kind = Var holder'; ty; loc} in
           {kind = Seq {nodes = [if_stmt; var]}; ty; loc}
          ))

  | Mir.Ast.{kind = ExprCall {func = {kind = Var f}; args}; ty; loc} ->
     let rec bind xs args =
       match args with
       | [] -> {kind= Call {name=f; args = List.rev xs}; ty; loc}
       | a :: args ->
          let k = insert_let (generate env a) in
          k (fun k -> bind (k :: xs) args)
     in
     bind [] args

  | Mir.Ast.{kind = ExprCall {func; args}; ty; loc} ->
     failwith "[ICE] not supported"

  | Mir.Ast.{kind = LitNum n; ty; loc} ->
     {kind = Num n; ty; loc}

  | Mir.Ast.{kind = LitUnit; ty; loc} ->
     {kind = Unit; ty; loc}

  | Mir.Ast.{kind = Var s; ty; loc} ->
     {kind = Var s; ty; loc}
