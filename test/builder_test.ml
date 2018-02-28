module Ir_builder = struct
  type t = unit

  type module_t = unit
  type type_t = unit

  let default () =
    ()

  let make_module builder =
    ()

  let make_type name builder =
    ()
end


let () =
  let builder = Ir_builder.default () in
  let m = Ir_builder.make_module () in
  let ty = Ir_builder.make_type "" in
  let _ = Ir_builder.add_member "" ty ty in
