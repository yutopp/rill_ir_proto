(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

%start <Ast.t> program_entry

%{
  let to_loc spos epos =
    let open Lexing in
    let p = Loc.{
      pos_begin_cnum = spos.pos_cnum;
      pos_begin_lnum = spos.pos_lnum;
      pos_begin_bol  = spos.pos_cnum - spos.pos_bol;
      pos_end_cnum   = epos.pos_cnum;
      pos_end_lnum   = epos.pos_lnum;
      pos_end_bol    = epos.pos_cnum - epos.pos_bol;
    } in
    Some p

  let wrap spos epos kind =
    let loc = to_loc spos epos in
    Ast.{kind; loc}
%}

%%

program_entry:
    module_spec
    definitions
    EOF
    { Ast.Module $2 |> wrap $startpos $endpos }

module_spec:
    KEYWORD_MODULE lit_string
    { Ast.SpecModule $2 |> wrap $startpos $endpos }

definitions:
    list(definition) { $1 }

definition:
    definition_function
    { $1 }

definition_function:
    KEYWORD_DEF
    id
    function_parameter_list
    function_ret_type
    function_body
    { Ast.DefFunc {id = $2; params = $3; ret_spec = $4; body = $5} |> wrap $startpos $endpos }

function_parameter_list:
    LPAREN
    separated_list(COMMA, function_parameter_decl)
    RPAREN
    { $2 }

function_parameter_decl:
    id
    COLON
    type_spec
    { Ast.DeclParam {id = $1; ty_spec = $3} |> wrap $startpos $endpos }

function_ret_type:
    { Ast.TypeSpec (Ast.Id "unit" |> wrap $startpos $endpos) |> wrap $startpos $endpos }
  | type_spec { $1 }

function_body:
    expression_block { $1 }

type_spec:
    id
    { Ast.TypeSpec $1 |> wrap $startpos $endpos }

expression:
    separated_nonempty_list(SEMICOLON, expression_cond)
    { Ast.ExprSeq $1 |> wrap $startpos $endpos }

expression_cond:
    expression_if { $1 }
  | expression_binary { $1 }

expression_block:
    LBLOCK
    expression
    RBLOCK
    { Ast.ExprBlock $2 |> wrap $startpos $endpos }
  | LBLOCK
    RBLOCK
    { Ast.ExprBlock (Ast.LitUnit |> wrap $startpos($2) $endpos($2)) |> wrap $startpos $endpos }

expression_if:
    KEYWORD_IF
    cond_e = expression
    then_e = expression_block
    { Ast.ExprIf {cond = cond_e; then_c = then_e; else_c = None} |> wrap $startpos $endpos}
  | KEYWORD_IF
    cond_e = expression
    then_e = expression_block
    KEYWORD_ELSE
    else_e = expression_block
    { Ast.ExprIf {cond = cond_e; then_c = then_e; else_c = Some else_e} |> wrap $startpos $endpos }

expression_binary:
    expression_binary id_binary_op expression_primary
    { Ast.ExprBinOp {op = $2; lhs = $1; rhs = $3} |> wrap $startpos $endpos }
  | expression_unary
    { $1 }

expression_unary:
    expression_postfix { $1 }

expression_postfix:
    expression_postfix expression_argument_list
    { Ast.ExprCall {func = $1; args = $2} |> wrap $startpos $endpos }

  | expression_primary
    { $1 }

expression_argument_list:
    LPAREN
    separated_list(COMMA, expression)
    RPAREN
    { $2 }

expression_primary:
  | LPAREN expression RPAREN { $2 }
  | lit_string { $1 }
  | lit_int { $1 }
  | lit_bool { $1 }
  | lit_unit { $1 }
  | id { $1 }

id:
    ID
    { Ast.Id $1 |> wrap $startpos $endpos}

id_binary_op:
    BIN_OP
    { Ast.Id $1 |> wrap $startpos $endpos}

lit_string:
    STRING
    { Ast.LitString $1 |> wrap $startpos $endpos }

lit_int:
    INT
    { let (value, bits, signed) = $1 in
      Ast.LitInt {value; bits; signed} |> wrap $startpos $endpos
    }

lit_bool:
    KEYWORD_TRUE
    { Ast.LitInt {value = 1; bits = 1; signed = false} |> wrap $startpos $endpos }
  | KEYWORD_FALSE
    { Ast.LitInt {value = 0; bits = 1; signed = false} |> wrap $startpos $endpos }

lit_unit:
    LPAREN RPAREN
    { Ast.LitUnit |> wrap $startpos $endpos }
