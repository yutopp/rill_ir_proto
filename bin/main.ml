(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Compiler = struct
  let compile_file in_path out_path =
    let open Rill_ir in
    Printf.printf "compile_file %s -> %s\n" in_path out_path;
    let res = Syntax.make_ast_from_file in_path in
    match res with
    | Ok tree ->
       let ctx = Context.empty () in
       let env = Env.empty () in
       let (typed_tree, env) = Typing.generate ctx env tree in
       begin
         match Context.get_errors ctx with
         | [] ->
            let k_form = typed_tree |> K_normal.generate env in
            let ir_ctx = Ir.make_context () in

            let p m =
              m |> Ir.show |> Printf.printf "IR: %s\n"
            in
            let rill_module = k_form |> Ir.generate ir_ctx env in
            let () = rill_module |> p in

            let rill_module = rill_module |> Ir.apply_pass ~tap:p ir_ctx env in

            let backend_ctx = Backend.make_context () in
            let m = rill_module |> Backend.generate backend_ctx in
            let () = m |> Backend.show |> Printf.printf "LLVM:\n%s\n" in
            let () = Backend.validate m |> Option.may (Printf.printf "%s\n") in

            let () = Backend.emit_file backend_ctx m out_path in

            Ok ()
         | errors ->
            errors |> List.map Error_msg.to_msg |> List.iter (Printf.printf "%s\n");
            Ok ()
       end
    | Bad e ->
       Bad e
end

let build co =
  match co.Args.compile_only with
  | true ->
     let in_path = match co.Args.input_files with
       | [p] -> p
       | _   -> failwith "Expect: single file"
     in
     let out_path =
       let p = co.Args.output_file |> Option.default in_path in
       Filetype.convert_path (Filetype.Llvm L_asm) p
     in
     let _ = Compiler.compile_file in_path out_path in
     ()

  | false ->
     let inout_paths =
       co.Args.input_files
       |> List.map (fun in_path ->
                    (in_path, Filetype.convert_path (Filetype.Obj) in_path)
                   )
     in
     let results =
       inout_paths
       |> List.map (fun (in_path, out_path) -> Compiler.compile_file in_path out_path)
     in
     ()

let is_successful res =
  true

let () =
  let co = Args.parse () in
  let res = build co in
  match is_successful res with
  | true ->
     exit 0
  | false ->
     exit 1
