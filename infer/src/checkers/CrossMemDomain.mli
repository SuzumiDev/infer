open! IStd

include AbstractDomain.S

val initial : t

type allocType = AllocC | AllocRust | FreeC | FreeRust | Incompatible

val len : t -> int
val add : t -> string -> allocType -> t
val remove : t -> string -> t
val mem : t -> string -> bool
val find : t -> string -> allocType
val rename_key : t -> string -> string -> t
val val_exists : allocType -> t -> bool

type summary = t
