open Batteries
module L = Llvm

type t = {
  llmodule: Llvm.llmodule
}

type storage_t =
  | StoImm
  | StoStack
  | StoIgnore
[@@deriving show]

type env_value_t = {ll: L.llvalue option; sto: storage_t}

type env_t = (Id_string.t, env_value_t) Map.t

type context_t = {
  llcontext: L.llcontext;
  llbuilder: L.llbuilder;
}

let initialize_backends =
  let is_initialized = ref false in
  let initialize () =
    match !is_initialized with
    | false ->
       Llvm_all_backends.initialize ();

       is_initialized := true
    | true ->
       ()
  in
  initialize

let is_stack_type ty =
  false

let rec to_lltype ctx m ty =
  let llctx = ctx.llcontext in
  match ty.Mir.Type_ref.state with
  | Mir.Type_ref.Complete (Mir.Type_ref.Primitive (Mir.Type_ref.Int {bits})) ->
     L.integer_type llctx bits
  | Mir.Type_ref.Complete (Mir.Type_ref.Primitive Mir.Type_ref.Unit) ->
     L.void_type llctx
  | Mir.Type_ref.Complete (Mir.Type_ref.Function (param_tys, ret_ty)) ->
     let param_ll_tys = param_tys |> List.map (to_lltype ctx m) in
     let ret_ll_ty = ret_ty |> to_lltype ctx m in
     L.function_type ret_ll_ty (param_ll_tys |> Array.of_list)
  | _ ->
     failwith "[ICE] not supported type"

let make_context () =
  let () = initialize_backends () in

  let context = L.global_context () in
  let builder = L.builder context in
  {
    llcontext = context;
    llbuilder = builder;
  }

let rec generate_type ctx m name ty llmod parent_llty_opt =
  ()

let generate_static_proto ctx m name v llmod parent_llty_opt =
  match v.Mir.Var_ref.kind with
  | Mir.Var_ref.Undefined ->
     failwith "[ICE] still unknown"

  | Mir.Var_ref.Expr {kind = Mir.Ast.ExprFunc _; ty; loc} ->
     let fty = to_lltype ctx m ty in
     let _llfv = L.declare_function name fty llmod in
     ()

  | Mir.Var_ref.Expr e ->
     failwith "not supported yet"

let rec generate_static ctx m name v llmod parent_llty_opt =
  match v.Mir.Var_ref.kind with
  | Mir.Var_ref.Undefined ->
     failwith "[ICE] still unknown"

  | Mir.Var_ref.Expr ({kind = Mir.Ast.ExprFunc _; ty; loc} as e) ->
     let env = Frontend_env.empty () in
     let k = K_normal.generate env e in

     let passes = [
       Ir.complete_pass';
       Ir.reduce_tmp_vars_pass';
       Ir.collect_stack_pass'
     ] in

     let ir_ctx = Ir.make_context () in
     let ir =
       let base = Ir.generate' ir_ctx env k in
       let () = base |> Ir.show_value |> Printf.printf "IR0: %s\n" in
       List.fold_left (fun e pass ->
                       let e' = pass ir_ctx env e in
                       let () = e' |> Ir.show_value |> Printf.printf "IRN: %s\n" in
                       e'
                      ) base passes
     in
     flush_all ();

     let env = Map.empty in
     let _ = generate_value ~recv_var:name ctx llmod env ir in
     ()

  | Mir.Var_ref.Expr _ ->
     failwith "not supported yet"

and generate ctx m =
  let name = "Rill" in (* TODO: change to ir_module name *)
  let llmod = L.create_module ctx.llcontext name in

  let () =
    let build name t =
      generate_type ctx m name t llmod None
    in
    Hashtbl.iter build m.Mir.tenv
  in

  let () =
    let build name v =
      generate_static_proto ctx m name v llmod None
    in
    Hashtbl.iter build m.Mir.venv
  in

  let () =
    let build name v =
      generate_static ctx m name v llmod None
    in
    Hashtbl.iter build m.Mir.venv
  in

  {
    llmodule = llmod;
  }

and generate_value ~recv_var ctx llmod env ir_value =
  let open Ir in
  match ir_value with
  | {kind = Function ({name; params; basic_blocks; _}, vars)} ->
     let f = L.lookup_function recv_var llmod |> Option.get in
     let () =
       List.iter2 L.set_value_name params (L.params f |> Array.to_list);
     in

     let env =
       List.fold_lefti (fun e index p ->
                        let llp = L.param f index in
                        let ev = {ll = Some llp; sto = StoImm} in
                        Map.add p ev e
                       ) env params
     in

     (* entry *)
     let entry_bb = L.append_block ctx.llcontext "entry" f in
     L.position_at_end entry_bb ctx.llbuilder;

     let env =
       VarsMap.fold
         (fun k v acc ->
          let storage = match v with
            | (Mir.Type_ref.{state = Complete (Primitive Unit)}, _) -> StoIgnore (* unit will not be exported to executable *)
            | (_, Ir.MutVar) -> StoStack
            | (ty, Ir.MutImm) when is_stack_type ty -> StoStack
            | (_ , Ir.MutImm) -> StoImm
          in
          Printf.printf "VAR %s, storage=%s\n" k (show_storage_t storage);
          let ev = match storage with
            | StoStack ->
               let llty = L.i32_type ctx.llcontext in (* TODO fix *)
               let llv = L.build_alloca llty k ctx.llbuilder in
               {ll = Some llv; sto = storage}
            | _ ->
               {ll = None; sto = storage}
          in
          Map.add k ev acc
         ) vars env
     in

     (* basic blocks *)
     let bb_env =
       Vect.fold_left
         (fun e bb ->
          let {index} = bb in
          let name = "" in
          let llbb = L.append_block ctx.llcontext name f in
          Map.add index llbb e
         ) Map.empty basic_blocks
     in

     (* connect entry and the first basic block *)
     L.position_at_end entry_bb ctx.llbuilder;
     let _ = L.build_br (Map.find 0 bb_env) ctx.llbuilder in

     (* actual instructions *)
     let _ =
       Vect.fold_left
         (fun e bb ->
          let {index; insts; terminator} = bb in

          let llbb = Map.find index bb_env in
          L.position_at_end llbb ctx.llbuilder;

          let next_e =
            Vect.fold_left
              (fun e i ->
               match generate_inst ctx llmod e i with
               | Some (_, ne) -> ne
               | None -> e
              ) e insts
          in
          Option.may (fun t ->
                      generater_terminator ctx llmod next_e bb_env t |> ignore
                     ) terminator;

          next_e
         ) env basic_blocks
     in
     L.dump_value f;
     flush_all ();
     f

  | {kind = IntValue v; ty} ->
     L.const_int (to_lltype ctx llmod ty) v

  | {kind = UndefValue} ->
     (* FIX *)
     L.const_int (L.i32_type ctx.llcontext) 0

  (* TODO: FIX *)
  | {kind = Call ("=", [lhs; rhs])} ->
     let {ll; sto} = Map.find lhs env in
     let lhs_v = Option.get ll in
     let {ll; sto} = Map.find rhs env in
     let rhs_v = Option.get ll in
     L.build_add lhs_v rhs_v "" ctx.llbuilder

  (* TODO: FIX *)
  | {kind = Call ("+", [lhs; rhs])} ->
     let {ll; sto} = Map.find lhs env in
     let lhs_v = Option.get ll in
     let {ll; sto} = Map.find rhs env in
     let rhs_v = Option.get ll in
     L.build_add lhs_v rhs_v "" ctx.llbuilder

  (* TODO: FIX *)
  | {kind = Call ("*", [lhs; rhs])} ->
     let {ll; sto} = Map.find lhs env in
     let lhs_v = Option.get ll in
     let {ll; sto} = Map.find rhs env in
     let rhs_v = Option.get ll in
     L.build_mul lhs_v rhs_v "" ctx.llbuilder

  | {kind = Call (func_name, arg_names)} ->
     Printf.printf "FIND=%s\n" func_name;
     let f = L.lookup_function func_name llmod |> Option.get in

     let args =
       arg_names
       |> List.map (fun n -> Map.find n env)
       |> List.map (fun {ll; sto} -> Option.get ll)
       |> Array.of_list
     in

     L.build_call f args "" ctx.llbuilder

  | {kind = Var (var_name)} ->
     let {ll; sto} = Map.find var_name env in
     Option.get ll

  | {kind = Unit} ->
     L.undef (L.void_type ctx.llcontext)

and generate_inst ctx m env inst =
  match inst with
  | Ir.Let (name, v) ->
     let {ll; sto} = Map.find name env in
     Printf.printf "LET %s: %s\n" name (show_storage_t sto);
     let llvalue = generate_value ~recv_var:name ctx m env v in
     let ret = match sto with
       | StoImm ->
          L.set_value_name name llvalue;
          let ev = {ll = Some llvalue; sto = StoImm} in
          let next_env = Map.add name ev env in
          Some (llvalue, next_env)
       | StoStack ->
          let recv_v = Option.get ll in
          let st = L.build_store llvalue recv_v ctx.llbuilder in
          Some (st, env)
       | StoIgnore ->
          None
     in
     ret
  | Ir.Assign (name, v) ->
     Printf.printf "LET %s\n" name;
     let {ll; sto} = Map.find name env in
     let llvalue = generate_value ~recv_var:name ctx m env v in
     let ret = match sto with
       | StoImm ->
          failwith ""
       | StoStack ->
          let recv_v = Option.get ll in
          let st = L.build_store llvalue recv_v ctx.llbuilder in
          Some (st, env)
       | StoIgnore ->
          None
     in
     ret
  | Ir.Nop ->
     None

and generater_terminator ctx m env bb_env terminator =
  match terminator with
  | Ir.Jump index ->
     let next_llbb = Map.find index bb_env in
     L.build_br next_llbb ctx.llbuilder

  | Ir.Cond (cond, then_index, else_index) ->
     let {ll; sto} = Map.find cond env in
     let llcond = Option.get ll in
     let then_llbb = Map.find then_index bb_env in
     let else_llbb = Map.find else_index bb_env in
     L.build_cond_br llcond then_llbb else_llbb ctx.llbuilder

  | Ir.Ret e ->
     let {ll; sto} = Map.find e env in
     let term = match (ll, sto) with
       | (Some llv, StoImm) ->
          L.build_ret llv ctx.llbuilder
       | (Some llv, StoStack) ->
          let llv = L.build_load llv "" ctx.llbuilder in
          L.build_ret llv ctx.llbuilder
       | (None, _) ->
          L.build_ret_void ctx.llbuilder
       | _ ->
          failwith "[ICE]"
     in
     term

let show m =
  let {llmodule} = m in
  L.string_of_llmodule llmodule

let validate m =
  let {llmodule} = m in
  Llvm_analysis.verify_module llmodule

let emit_file ctx m out_path =
  let ll = show m in
  File.with_file_out out_path (fun f -> IO.write_string f ll)
