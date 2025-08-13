open! IStd
module F = Format 

type t = int

let leq ~lhs ~rhs = lhs <= rhs

let join a b = max a b

let widen ~prev ~next ~num_iters:_ = join prev next

let pp fmt astate = F.fprintf fmt "%d" astate

let initial = 0
type summary = t