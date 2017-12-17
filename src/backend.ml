open Batteries
module L = Llvm

type storage_t =
  | StoImm
  | StoStack

type env_value_t = {ll: L.llvalue option; sto: storage_t}

type env_t = (Id_string.t, env_value_t) Map.t

type context_t = {
  llcontext: L.llcontext;
  llbuilder: L.llbuilder;
}

let is_stack_type ty =
  false

let initialize_backends =
  let is_initialized = ref false in
  let initialize () =
    match !is_initialized with
    | false ->
       Llvm_X86.initialize ();
       is_initialized := true
    | true ->
       ()
  in
  initialize

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
  m

and generate_global_interfaces ctx m env ir_value =
  let open Ir in
  match ir_value with
  | {kind = Function ({name; params; _}, _vars)} ->
     let param_tys = params |> List.map (fun _p -> L.i32_type ctx.llcontext) in
     let ret_ty = L.i32_type ctx.llcontext in
     let fty = L.function_type ret_ty (param_tys |> Array.of_list) in

     let f = L.declare_function "" fty m in
     List.iter2 (fun lp p ->
                 L.set_value_name p lp
                ) (L.params f |> Array.to_list) params;

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
          let is_stack = match v with
            | (_, true) -> true
            | (ty, _) -> is_stack_type ty
          in
          Printf.printf "VAR %s, is_stack=%b\n" k is_stack;
          let ev = match is_stack with
            | true ->
               let llty = L.i32_type ctx.llcontext in
               let llv = L.build_alloca llty k ctx.llbuilder in
               {ll = Some llv; sto = StoStack}
            | false ->
               {ll = None; sto = StoImm}
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

  | {kind = IntValue v} ->
     L.const_int (L.i32_type ctx.llcontext) v

  | {kind = BoolValue b} ->
     L.const_int (L.i1_type ctx.llcontext) (if b then 1 else 0)

  | {kind = UndefValue} ->
     (* FIX *)
     L.const_int (L.i32_type ctx.llcontext) 0

  | {kind = Call ("+", [lhs; rhs])} ->
     (* TODO: FIX *)
     let {ll; sto} = Map.find lhs env in
     let lhs_v = Option.get ll in
     let {ll; sto} = Map.find rhs env in
     let rhs_v = Option.get ll in
     L.build_add lhs_v rhs_v "" ctx.llbuilder

  | {kind = Call (func_name, arg_names)} ->
     failwith ""

  | {kind = Var (var_name)} ->
     let {ll; sto} = Map.find var_name env in
     Option.get ll

  | {kind = Unit} ->
     L.undef (L.void_type ctx.llcontext)

and generate_inst ctx m env inst =
  match inst with
  | Ir.Let (name, v) ->
     Printf.printf "LET %s\n" name;
     let {ll; sto} = Map.find name env in
     let llvalue = generate_value ~recv_var:name ctx m env v in
     L.set_value_name name llvalue;
     let ret = match sto with
       | StoImm ->
          let ev = {ll = Some llvalue; sto = StoImm} in
          let next_env = Map.add name ev env in
          Some (llvalue, next_env)
       | StoStack ->
          let recv_v = Option.get ll in
          let st = L.build_store llvalue recv_v ctx.llbuilder in
          Some (st, env)
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
     let llv = Option.get ll in
     L.build_ret llv ctx.llbuilder

let show m =
  L.string_of_llmodule m
