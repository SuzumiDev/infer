open! IStd

include AbstractDomain.S

val initial : t

type allocType = AllocC | AllocRust | FreeC | FreeRust

val len : t -> int
val add : t -> string -> allocType -> t
val remove : t -> string -> t
val mem : t -> string -> bool
val find : t -> string -> allocType

type summary = t
