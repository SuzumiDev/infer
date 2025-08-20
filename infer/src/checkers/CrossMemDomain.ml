open! IStd
module F = Format 
module L = Logging
module StringMap = Stdlib.Map.Make(String);;

type t = string StringMap.t

let leq ~lhs ~rhs = StringMap.cardinal lhs <= StringMap.cardinal rhs

let join a b = StringMap.union (fun _ v1 v2 -> if v1 == v2 then Some v1 else (L.internal_error "what"; None)) a b

let widen ~prev ~next ~num_iters:_ = join prev next

let len astate = StringMap.cardinal astate

let add astate str value = astate |> StringMap.add str value

let remove astate str = astate |> StringMap.remove str

let mem astate str = StringMap.mem str astate

let pp fmt astate = F.fprintf fmt "%s" (String.concat (List.map (StringMap.bindings astate) ~f:(fun (k, v) -> "(" ^ k ^ ": " ^ v ^ ")")) ~sep:", ")

let initial = StringMap.empty
type summary = t