open Expr ;;

module Environment : sig
  type t
  val empty : t
end

exception Eval_error of string

val eval : Environment.t -> expression -> Environment.t * expression
