(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Compiler = struct
  module Env = Rill_ir.Frontend_env

  type t = {
    diagnostics: Rill_ir.Error.t list;
  }

  let is_succeeded result =
    List.length result.diagnostics = 0

  let compile_file in_path out_path =
    let open Rill_ir in
    Printf.printf "compile_file %s -> %s\n" in_path out_path;
    let ctx = Context.empty () in

    let res = Syntax.make_ast_from_file ctx in_path in
    match res with
    | Some tree ->
       let () = tree |> Ast.show |> Printf.printf "TREE:\n%s\n" in

       let env = Env.empty () in
       let (typed_tree, env) = Typing.generate ctx env tree in
       let () = typed_tree |> T_ast.show |> Printf.printf "TYPED TREE:\n%s\n" in

       begin
         match Context.get_errors ctx with
         | [] ->
            let mm = Builder.build typed_tree in

            let backend_ctx = Backend.make_context () in
            let m = Backend.generate backend_ctx mm in
            let () = m |> Backend.show |> Printf.printf "LLVM:\n%s\n" in
            let () = Backend.validate m |> Option.may (Printf.printf "%s\n") in

            let () = Backend.emit_file backend_ctx m out_path in

            {
              diagnostics = []
            }
         | errors ->
            {
              diagnostics = errors
            }
       end
    | None ->
       let errs = Context.get_errors ctx in
       {
         diagnostics = errs
       }
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
     let result = Compiler.compile_file in_path out_path in
     [result]

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
     results

let () =
  Printexc.record_backtrace true;

  let co = Args.parse () in
  let results = build co in
  match List.for_all Compiler.is_succeeded results with
  | true ->
     exit 0
  | false ->
     List.iter (fun result ->
                List.iter (fun diagnostics ->
                           Printf.printf "%s\n" (Rill_ir.Error_msg.to_msg diagnostics)
                          ) result.Compiler.diagnostics
               ) (results |> List.filter (Compiler.is_succeeded %> not));
     exit 1
