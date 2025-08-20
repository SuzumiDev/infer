open! IStd

include AbstractDomain.S

val initial : t

val len : t -> int
val add : t -> string -> string -> t
val remove : t -> string -> t
val mem : t -> string -> bool

type summary = t