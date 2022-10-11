module Environment : sig
  type t

  val empty : t
end

exception Error of string

val eval : Environment.t -> expression -> Environment.t * expression
