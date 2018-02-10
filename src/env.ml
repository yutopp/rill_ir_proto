(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module StringMap = Map.String

type t = {
  venv: Type.t StringMap.t;
  tenv: Type.t StringMap.t;
}

let empty () =
  {
    venv = StringMap.empty;
    tenv = StringMap.empty;
  }

let add_venv id ty env =
  { env with
    venv = StringMap.add id ty env.venv }

let find_venv id env =
  match StringMap.find id env.venv with
  | x -> Some x
  | exception Not_found -> None

let add_tenv id ty env =
  { env with
    tenv = StringMap.add id ty env.tenv }

let find_tenv id env =
  match StringMap.find id env.tenv with
  | x -> Some x
  | exception Not_found -> None

let dump env =
  let s = "venv\n" in
  let s =
    StringMap.fold (fun k v s ->
                    s ^ Printf.sprintf " - '%s':%s\n" k (Type.show v)
                   ) env.venv s
  in
  let s = s ^ "tenv\n" in
  let s =
    StringMap.fold (fun k v s ->
                    s ^ Printf.sprintf " - '%s':%s\n" k (Type.show v)
                   ) env.tenv s
  in
  Printf.printf "%s" s
