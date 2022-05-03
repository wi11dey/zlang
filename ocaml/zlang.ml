let print (exp : expression) : unit =
  print_string "== " ;
  print_endline (write true exp) ;;

let prompt () : char Stream.t Seq.node =
  print_string "z> ";
  Seq.Cons (Stream.of_string (read_line ()), prompt) ;;

let repl (env : environment) : char Stream.t Seq.t -> unit =
  Seq.iter (fun input ->
      input
      |> read
      |> eval env
      |> print) ;;

let main =
  let scripts =
    Seq.map (fun file ->
        Printf.printf "Running %s...\n";
        file |> open_in |> Stream.of_channel)
      (tail (Array.to_seq Sys.argv)) in

  let env = Environment.create (List.length stdlib) in

  List.iter (eval env) stdlib;

  try
    repl env (Seq.append scripts prompt)
  with
  | Invalid_syntax (pos, msg) ->
     Printf.eprintf "syntax error at %d: %s\n" pos msg
  | Error msg ->
     Printf.eprintf "error: %s\n" msg ;;
