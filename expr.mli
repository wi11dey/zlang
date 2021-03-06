exception Syntax_error of int * string

type expression =
  | Boolean of bool
  | Symbol of string
  | Character of char
  | Vector of expression list
  | Pair of expression * expression
  | Integer of int
  | Real of float
  | String of string
  | Procedure of (expression list -> expression)
  | This
  | Empty ;;

val write : bool -> expression -> string

type source = char Stream.t
val read : source -> expression
