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

let rec ll_of_ty ctx ty =
  let llctx = ctx.llcontext in
  match ty with
  | Type.Primitive (Int {bits}) ->
     L.integer_type llctx bits
  | Type.Primitive Unit ->
     L.void_type llctx
  | Type.Function (param_tys, ret_ty) ->
     let param_ll_tys = param_tys |> List.map (ll_of_ty ctx) in
     let ret_ll_ty = ret_ty |> ll_of_ty ctx in
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

let rec generate ctx ir_module =
  let name = "Rill" in      (* TODO: change to ir_module name *)
  let m = L.create_module ctx.llcontext name in
  let Ir.{functions} = ir_module in

  let env =
    List.fold_left (fun e g ->
                    generate_global_interfaces ctx m e g
                   ) Map.empty functions
  in
  List.iter (fun f ->
             generate_value ~recv_var:"" ctx m env f |> ignore
            ) functions;
  {
    llmodule = m
  }

and generate_global_interfaces ctx m env ir_value =
  let open Ir in
  match ir_value with
  | {kind = Function ({name; params; _}, _vars); ty} ->
     Printf.printf "FUNC=%s\n" name;
     let fty = ll_of_ty ctx ty in

     let f = L.declare_function name fty m in
     List.iter2 L.set_value_name params (L.params f |> Array.to_list);

     let ev = {ll = Some f; sto = StoImm} in
     Map.add name ev env

  | _ ->
     env

and generate_value ~recv_var ctx m env ir_value =
  let open Ir in
  match ir_value with
  | {kind = Function ({name; params; basic_blocks; _}, vars)} ->
     let {ll; sto} = Map.find name env in
     let f = Option.get ll in
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
            | (Type.Primitive Unit, _) -> StoIgnore (* unit will not be exported to executable *)
            | (_, Ir.MutVar) -> StoStack
            | (ty, Ir.MutImm) when is_stack_type ty -> StoStack
            | (_ , Ir.MutImm) -> StoImm
          in
          Printf.printf "VAR %s, storage=%s\n" k (show_storage_t storage);
          let ev = match storage with
            | StoStack ->
               let llty = L.i32_type ctx.llcontext in
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
               match generate_inst ctx m e i with
               | Some (_, ne) -> ne
               | None -> e
              ) e insts
          in
          Option.may (fun t ->
                      generater_terminator ctx m next_e bb_env t |> ignore
                     ) terminator;

          next_e
         ) env basic_blocks
     in
     f

  | {kind = IntValue v; ty} ->
     L.const_int (ll_of_ty ctx ty) v

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
     let {ll; sto} = Map.find func_name env in
     let lhs_v = Option.get ll in

     L.build_call lhs_v [||] "" ctx.llbuilder

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
