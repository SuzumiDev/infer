open! IStd
module F = Format 
module L = Logging
module StringMap = Stdlib.Map.Make(String);;


type allocType = AllocC | AllocRust | FreeC | FreeRust | Incompatible [@@deriving equal]

type t = allocType StringMap.t

let leq ~lhs ~rhs = StringMap.cardinal lhs <= StringMap.cardinal rhs

let join a b = StringMap.union (fun _ v1 v2 -> if v1 == v2 then Some v1 else (L.internal_error "what"; None)) a b

let widen ~prev ~next ~num_iters:_ = join prev next

let len astate = StringMap.cardinal astate

let add astate str value = astate |> StringMap.add str value

let remove astate str = astate |> StringMap.remove str

let mem astate str = StringMap.mem str astate

let find astate str = StringMap.find str astate

let rename_key astate old_key new_key =
  match StringMap.find_opt old_key astate with
    | None -> astate
    | Some v -> let astate1 = remove astate old_key in
                add astate1 new_key v

let val_exists value = StringMap.exists (fun _ v -> equal_allocType value v)

let pp_alloc a = match a with
  | AllocC -> "AllocC"
  | AllocRust -> "Allocrust"
  | FreeC -> "FreeC"
  | FreeRust -> "FreeRust"
  | Incompatible -> "Incompatible"

let pp fmt astate = F.fprintf fmt "%s" (String.concat (List.map (StringMap.bindings astate) ~f:(fun (k, v) -> "(" ^ k ^ ": " ^ (pp_alloc v) ^ ")")) ~sep:", ")

let initial = StringMap.empty
type summary = t
