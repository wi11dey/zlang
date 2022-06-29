open Expr ;;
open Eval ;;
open Core ;;

let repl (env : Environment.t) (inputs : source Seq.t) : Environment.t =
  let rec read_all (src : source) () : expression Seq.node =
    try
      Seq.Cons (read src, read_all src)
    with
    | Syntax_error (pos, msg) ->
       Printf.eprintf "syntax error at position %d: %s\n%!" pos msg;
       Seq.Nil
    | Stream.Failure ->
       Seq.Nil in
  Seq.fold_left (fun env exp ->
      try
        let env', exp = eval env exp in
        print_string "== ";
        print_endline (write true exp);
        env'
      with Eval_error msg ->
        Printf.eprintf "error: %s\n%!" msg;
        env) env (Seq.flat_map read_all inputs) ;;

let rec prompt () : source Seq.node =
  print_string "z> ";
  Seq.Cons (Stream.of_string (read_line ()), prompt) ;;

let main =
  let scripts =
    Seq.map (fun file ->
        if file = "-q" then exit 0;
        Printf.printf "Running %s...\n%!" file;
        Stream.of_channel (open_in file))
      (Sys.argv
       |> Array.to_list
       |> List.tl
       |> List.to_seq) in
  let env = List.fold_left (fun env' exp -> fst (eval env' exp))
              Environment.empty core in
  repl env (Seq.append scripts prompt) ;;
