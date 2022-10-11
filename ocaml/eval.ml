module Environment = struct
  type t = (string * expression) list

  let empty : t = []

  let define (name : string) (value : expression) (env : t) : t =
    (name, value) :: env

  let lookup : string -> t -> expression option = List.assoc_opt
end ;;

exception Error of string ;;

let rec eval (env : Environment.t) (exp : expression) : Environment.t * expression =
  let this : expression = Procedure (fun { this } -> this) in

  let type_of (exp : expression) : string =
    | Boolean _ -> "boolean"
    | Character _ -> "character"
    | Vector _ -> "vector"
    | Pair _ -> "list"
    | Empty -> "list"
    | Integer _ -> "integer"
    | Real _ -> "real"
    | String _ -> "string"
    | _ ->
       (* Should not be a zlang Error, but rather an OCaml Failure, because it indicates there is something wrong with the OCaml code since control flow should never reach here.  *)
       failwith (write true exp)^" outside of type system" in

  let rec add_method (f : expression) (m : expression) : expression =
    match func with
    | Pair (Symbol "function") ->
    | _ -> Pair () in

  match exp with
  | Pair (Symbol "function", )
    | Pair (Symbol "define",
            Pair (Pair (Symbol "quote", wildcard), rest)) ->
  | Pair (Symbol "define", Pair (func, rest)) ->
  | Pair (Symbol "define", Pair (Symbol var, definition)) ->
     (match Environment.lookup var env with
      | Some (Pair (Symbol "function", body)) ->
      | Some _
      | None -> Environment.define var definition)
  | Pair (Symbol "define", _) ->
     raise (Error "cannot define non-symbol")
  | Symbol s ->
     (match Environment.lookup s env with
      | None -> raise (Error s^" not defined")
      | Some value -> env, value)
  | _ -> env, exp ;;
