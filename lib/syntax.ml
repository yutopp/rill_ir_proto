(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries.Result

exception LexingError of Lexing.position * Lexer.error_kind
exception ParsingError of Lexing.position

let make_lexedbuf_from_input input =
  input |> BatIO.to_input_channel |> Lexing.from_channel

let make_ast lexedbuf =
  try
    Ok (Parser.program_entry Lexer.token lexedbuf)
  with
  | Lexer.Error kind ->
     let pos = Lexing.lexeme_start_p lexedbuf in
     Bad (LexingError (pos, kind))

  | Parser.Error ->
     let pos = Lexing.lexeme_start_p lexedbuf in
     Bad (ParsingError pos)

let make_ast_from_file filepath =
  let make_ast_from_input input =
    input
    |> make_lexedbuf_from_input
    |> make_ast
  in
  Batteries.File.with_file_in filepath make_ast_from_input
