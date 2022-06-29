open Expr ;;

module Environment = struct
  type t = (string * expression) list

  let empty : t = []
end ;;

exception Eval_error of string ;;

let rec eval (env : Environment.t) (exp : expression)
        : Environment.t * expression =
  let rec seq_of_pair (pair : expression) () : expression Seq.node =
    match pair with
    | Pair (hd, tl) -> Seq.Cons (hd, seq_of_pair tl)
    | Empty -> Seq.Nil
    | _ -> raise (Eval_error "invalid list") in

  let rec substitute
            (original : expression)
            (replacement : expression)
            (exp : expression)
          : expression =
    if exp = original then replacement
    else match exp with
         | Pair (hd, tl) -> Pair (substitute original replacement hd,
                                  substitute original replacement tl)
         | Vector exps -> Vector (List.map (substitute original replacement)
                                    exps)
         | _ -> exp in

  let rec force (f : expression) (args : expression) : expression =
    match f with
    | Procedure p -> p (List.of_seq (seq_of_pair args))
    | Symbol "function" -> Pair (f, args)
    | Symbol "begin" ->
       let rec unwind (env' : Environment.t) (exp : expression) : expression =
         if env == env' then exp (* Physical equality is intentional. *)
         else match env' with
              | (name, value) :: tl ->
                 unwind tl (substitute (Symbol name) value exp)
              | [] ->
                 failwith "environments did not have a common ancestor" in
       let rec block (env' : Environment.t) (exps : expression) : expression =
         match exps with
         | Pair (last, Empty) -> unwind env' last
         | Pair (hd, tl) -> block (fst (eval env' hd)) tl
         | _ -> raise (Eval_error "invalid block") in
       block env args
    | _ ->
       let rec methods (f : expression) : (expression * expression) Seq.t =
         match eval env f with
         | _, Pair (Symbol "function",
                    Pair (Pair (Symbol "quasiquote",
                                signature), body)) ->
            let rec resolve (signature : expression) : expression =
              match signature with
              | Pair (Symbol var, rest) -> Pair (snd (eval env (Symbol var)),
                                                 resolve rest)
              | Pair (hd, tl) -> Pair (hd, resolve tl)
              | Empty -> Empty
              | _ -> raise (Eval_error "invalid method signature") in
            print_endline (write true (resolve signature));
            Seq.return (resolve signature, body)
         | _, Pair (Symbol "function", subfunctions) ->
            Seq.flat_map methods (seq_of_pair subfunctions)
         | _ -> raise (Eval_error ((write true f)^" is not callable")) in

       let wildcard (f : expression) : (expression * expression) Seq.t =
         try Seq.map (fun (signature, code) ->
                 substitute This f signature,
                 substitute This f code) (methods (List.assoc "" env))
         with Not_found -> Seq.empty in

       let typeof (exp : expression) : string option =
         match exp with
         | Boolean _ -> Some "boolean"
         | Character _ -> Some "character"
         | Vector _ -> Some "vector"
         | Pair (Symbol typ, _) -> Some typ
         | Integer _ -> Some "integer"
         | Real _ -> Some "real"
         | String _ -> Some "string"
         | _ -> None in

       let rec dispatch
                 (args : expression)
                 (candidates : (expression * expression) Seq.t)
               : expression option =
         match args with
         | Pair (current, rest) ->
            let literals (signature, code) =
              match signature with
              | Empty -> None
              | Pair (Pair (hd, _), _) when hd <> Symbol "function" -> None
              | Pair (literal, signature') when literal = current ->
                 Some (signature', code)
              | Pair _ -> None
              | _ -> raise (Eval_error "invalid method signature") in

            let typed (signature, code) =
              let bind (var : string) =
                Pair (Pair (Symbol "define",
                            Pair (Pair (Symbol "quote",
                                        Pair (Symbol var, Empty)),
                                  Pair (current, Empty))), code) in
              match signature with
              | Pair (Pair (Symbol "unquote",
                            Pair (Symbol var, Empty)),
                      signature') ->
                 Some (signature', bind var)
              | Pair (Pair (Symbol typ,
                            Pair (Pair (Symbol "unquote",
                                        Pair (Symbol var, Empty)), Empty)),
                      signature') when Some typ = typeof current ->
                 Some (signature', bind var)
              | Pair _
              | Empty -> None
              | _ -> raise (Eval_error "invalid method signature") in

            (match current, dispatch rest
                              (Seq.append
                                 (Seq.filter_map literals candidates)
                                 (Seq.filter_map typed candidates)) with
             | Pair (f', args'), None when f' <> Symbol "function" ->
                dispatch (Pair (force f' args', rest)) candidates
             | Symbol var, None ->
                (try dispatch (Pair (List.assoc var env, rest)) candidates
                 with Not_found -> raise (Eval_error (var^" not defined")))
             | _, None -> None
             | _, Some result -> Some result)
         | Empty ->
            let terminators (signature, code) =
              match signature with
              | Empty -> Some code
              | Pair _ -> None
              | _ -> raise (Eval_error "invalid method signature") in

            (match List.of_seq (Seq.filter_map terminators candidates) with
             | [] -> None
             | code :: [] -> Some (force (Symbol "begin") code)
             | _ -> raise (Eval_error "ambiguous method dispatch"))
         | _ -> raise (Eval_error "invalid function application") in
       try Option.get (dispatch args (Seq.append (methods f) (wildcard f)))
       with Invalid_argument _ -> raise (Eval_error "no matching method") in

  let rec validate_no_free_unquote (exp : expression) : unit =
    match exp with
    | Pair (Symbol "unquote", _)
      | Pair (Symbol "unquote-splicing", _) ->
       raise (Eval_error "unquote outside of quasiquotation context")
    | Pair (Symbol "quasiquote", _) -> ()
    | Pair (hd, tl) ->
       validate_no_free_unquote hd;
       validate_no_free_unquote tl
    | Vector exps ->
       List.iter validate_no_free_unquote exps
    | _ -> () in

  validate_no_free_unquote exp;
  let exp = substitute (Symbol "quote") (Symbol "quasiquote") exp in
  match exp with
  | Pair (Symbol "function", _) -> env, exp

  | Pair (Symbol "define",
          Pair (Pair (Symbol "quasiquote",
                      Pair (Pair (Symbol "unquote",
                                  Pair (Symbol wildcard,
                                        Empty)), Empty)),
                Pair (value, Empty))) ->
     let value' = substitute (Symbol wildcard) This value in
     (try let func = Pair (Symbol "function",
                           Pair (List.assoc "" env, Pair (value', Empty))) in
          ("", func) :: env, func
      with Not_found -> ("", value') :: env, value')
  | Pair (Symbol "define",
          Pair (Pair (Symbol "quasiquote",
                      Pair (Pair ((Pair (Symbol "unquote",
                                         Pair (Symbol _, Empty))
                                   | Symbol _) as f, arglist), Empty)),
                body)) ->
     eval env (Pair (Symbol "define",
                     Pair (Pair (Symbol "quasiquote",
                                 Pair (f, Empty)),
                           Pair (Pair (Symbol "function",
                                       Pair (Pair (Symbol "quasiquote",
                                                   arglist), body)), Empty))))
  | Pair (Symbol "define",
          Pair (Pair (Symbol "quasiquote",
                      Pair (Symbol name, Empty)),
                Pair (value, Empty))) ->
     (try let existing = List.assoc name env in
          (* Check if function-valued. *)
          match eval env existing, eval env value with
          | (_, Pair (Symbol "function", _)),
            (_, Pair (Symbol "function", _)) ->
             let f = Pair (Symbol "function",
                           Pair (existing, Pair (value, Empty))) in
             (name, f) :: env, f
          | _ -> raise Not_found
      with Not_found -> (name, value) :: env, value)
  | Pair (Symbol "define", _) -> raise (Eval_error "invalid definition")

  | Pair (f, args) -> eval env (force f args)

  | Symbol "quote"
    | Symbol "quasiquote"
    | Symbol "unquote"
    | Symbol "unquote-splicing" -> raise (Eval_error "reserved symbol")

  | Symbol s ->
     (try eval env (List.assoc s env)
      with Not_found -> raise (Eval_error (s^" not defined")))

  | Vector exps -> env, Vector (List.map (fun exp' -> snd (eval env exp')) exps)

  | This -> failwith "#<this> reached evaluation"

  | _ -> env, exp ;;
