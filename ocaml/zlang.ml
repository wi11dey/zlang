let print ((env, exp) : Environment.t * expression) : Environment.t =
  print_string "== " ;
  print_endline (write true exp);
  env ;;

let prompt () : source Seq.node =
  print_string "z> ";
  Seq.Cons (Stream.of_string (read_line ()), prompt) ;;

let repl (env : Environment.t) : source Seq.t -> unit =
  Seq.fold_left (fun env' input ->
      input
      |> read
      |> eval env'
      |> print) env ;;

let main =
  let scripts =
    Seq.map (fun file ->
        Printf.printf "Running %s...\n";
        Stream.of_channel (open_in file))
      (tail (Array.to_seq Sys.argv)) in
  let env = List.fold_left (fun env' exp -> fst (eval env' exp))
              Environment.empty stdlib in
  try repl env (Seq.append scripts prompt) with
  | Invalid_syntax (pos, msg) ->
     Printf.eprintf "syntax error at %d: %s\n" pos msg
  | Error msg ->
     Printf.eprintf "error: %s\n" msg ;;
