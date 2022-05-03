module Environment = struct
  module Definitions = Hashtbl.Make(struct
                           type t = string
                           let equal = (=)
                           let hash = Hashtbl.hash
                         end)

  include Definitions

  type t = expression Definitions.t
end;;

exception Error of string ;;

let rec eval (env : Environment.t) (exp : expression) =
  let this : expression = Procedure (fun { caller } -> caller) in

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
  | Pair (Symbol "define",
          Pair (Pair (Symbol "quote", wildcard), rest)) ->
     
  | Pair (Symbol "define", Pair (func, rest)) ->
  | Pair (Symbol "define", Symbol var) ->
     (match Hashtbl.find_opt env var with)
  | Pair (Symbol "define", unknown) ->
     raise (Error "cannot define non-symbol "^(write true unknown))
  | Symbol s ->
     (try Hashtbl.find env s
      with Not_found -> raise (Error s^" not defined"))
  | _ -> exp ;;
