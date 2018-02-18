(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

let run_test' filename =
  let open Rill_ir in
  let tree =
    let res = Syntax.make_ast_from_file filename in
    match res with
    | Ok t -> t
    | Bad e -> raise e
  in
  let () = tree |> Ast.show |> Printf.printf "TREE:\n%s\n" in

  let ctx = Context.empty () in
  let env = Env.empty () in
  let (typed_tree, env) = Typing.generate ctx env tree in
  let () = typed_tree |> T_ast.show |> Printf.printf "TYPED TREE:\n%s\n" in
  Context.get_errors ctx |> List.map Error_msg.to_msg |> List.iter (Printf.printf "%s\n");
  assert (not (Context.has_errors ctx));

  let k_form = typed_tree |> K_normal.generate env in
  let () = k_form |> K_normal.show |> Printf.printf "K NORMAL:\n%s\n" in

  let ir_ctx = Ir.make_context () in
  let rill_module = k_form |> Ir.generate ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "IR0: %s\n" in

  let rill_module = rill_module |> Ir.complete_pass ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "IR1: %s\n" in

  let rill_module = rill_module |> Ir.reduce_tmp_vars_pass ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "IR2: %s\n" in

  let rill_module = rill_module |> Ir.collect_stack_pass ir_ctx env in
  let () = rill_module |> Ir.show |> Printf.printf "IR3: %s\n" in

  let backend_ctx = Backend.make_context () in
  let m = rill_module |> Backend.generate backend_ctx in
  let () = m |> Backend.show |> Printf.printf "LLVM: %s\n" in
  let () = Backend.validate m |> Option.may (Printf.printf "%s\n") in
  Ok ()

let run_test filename =
  let open Rill_ir in
  try run_test' filename with
  | Syntax.ParsingError (pos) as e->
     Printf.printf "%s / %d:%d" filename pos.Lexing.pos_lnum pos.Lexing.pos_bol;
     Bad e

let () =
  Printexc.record_backtrace true;

  let test_file_regexp = Str.regexp ".*\\.rir" in
  let all_files = Sys.readdir "." |> Array.to_list in

  let files =
    all_files
    |> List.filter (fun s -> Str.string_match test_file_regexp s 0)
    |> List.sort compare
  in

  let results = files |> List.map run_test in

  ()
