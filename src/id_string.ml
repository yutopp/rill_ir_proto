type t = string
[@@deriving show]

let flesh =
  let count = ref 0 in
  let gen () =
    let n = !count in
    let () = incr count in
    Printf.sprintf "__tmp_%d" n
  in
  gen
