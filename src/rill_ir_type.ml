type t = {
  base: base_t;
  mutability: mutability_t;
}
 and base_t =
   | Concrete of tmp_kind_t
 and tmp_kind_t =
   | Module
   | Int32
   | Bool
   | Unit
   | Func of t list * t
 and mutability_t =
   | Immutable
   | Const
   | Mutable
[@@deriving show]
