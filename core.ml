open Expr ;;
open Eval ;;

let core =
  let defun (signature : string) (body : expression) =
    Pair (Symbol "define",
          Pair (read (Stream.of_string signature),
                Pair (body, Empty))) in
  [
    defun "`(+ (integer ,a) (integer ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Integer a :: Integer b :: [] -> Integer (a + b)
                 | _ -> failwith "wrong arguments to +"),
             read (Stream.of_string "(a b)")));
    defun "`(+ (real ,a) (real ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Real a :: Real b :: [] -> Real (a +. b)
                 | _ -> failwith "wrong arguments to +."),
             read (Stream.of_string "(a b)")));

    defun "`(- (integer ,a))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Integer a :: [] -> Integer (~-a)
                 | _ -> failwith "wrong arguments to ~-"),
             read (Stream.of_string "(a)")));
    defun "`(- (real ,a))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Real a :: [] -> Real (~-.a)
                 | _ -> failwith "wrong arguments to ~-."),
             read (Stream.of_string "(a)")));
    defun "`(- (integer ,a) (integer ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Integer a :: Integer b :: [] -> Integer (a - b)
                 | _ -> failwith "wrong arguments to -"),
             read (Stream.of_string "(a b)")));
    defun "`(- (real ,a) (real ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Real a :: Real b :: [] -> Real (a -. b)
                 | _ -> failwith "wrong arguments to -."),
             read (Stream.of_string "(a b)")));

    defun "`(* (integer ,a) (integer ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Integer a :: Integer b :: [] -> Integer (a * b)
                 | _ -> failwith "wrong arguments to *"),
             read (Stream.of_string "(a b)")));
    defun "`(* (real ,a) (real ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | Real a :: Real b :: [] -> Real (a *. b)
                 | _ -> failwith "wrong arguments to *."),
             read (Stream.of_string "(a b)")));

    defun "`(= (integer ,a) (integer ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | a :: b :: [] -> Boolean (a = b)
                 | _ -> failwith "wrong arguments to ="),
             read (Stream.of_string "(a b)")));
    defun "`(= (float ,a) (float ,b))"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | a :: b :: [] -> Boolean (a = b)
                 | _ -> failwith "wrong arguments to ="),
             read (Stream.of_string "(a b)")));

    defun "`(< ,a ,b)"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | a :: b :: [] -> Boolean (a < b)
                 | _ -> failwith "wrong arguments to ="),
             read (Stream.of_string "(a b)")));

    defun "`(write ,a)"
      (Pair (Procedure (fun lst ->
                 match lst with
                 | a :: [] -> String (write true a)
                 | _ -> failwith "wrong arguments to write"),
             read (Stream.of_string "(a)")))
  ] ;;
