exception Invalid_syntax of int * string ;;

type expression =
  | Boolean of bool
  | Symbol of string
  | Character of char
  | Vector of expression list
  | Pair of expression * expression
  | Integer of int
  | Real of float
  | String of string
  | Procedure of {
      caller: string;
      arguments: expression list option
    } -> expression
  | Empty ;;

let rec write (quoted : bool) (exp : expression) : string =
  match exp with
  | Boolean b -> if b then "#t" else "#f"
  | Symbol s -> s
  | Character c ->
     (match c with
      | ' ' -> "#\\space"
      | '\n' -> "#\\newline"
      | _ -> "#\\"^(String.make 1 c))
  | Vector lst ->
     "#("^(String.concat " " (List.map (write quoted) lst))^")"
  | Pair (head, tail) ->
     let rec write_tail (tail : expression) =
       match tail with
       | Empty -> ""
       | Pair (head2, tail2) -> " "^(write quoted head2)^(write_tail tail2)
       | _ -> " . "^(write quoted tail) in
     "("^(write quoted head)^(write_tail tail)^")"
  | Real f -> string_of_float f
  | Integer i -> string_of_int i
  | String s -> if quoted then "\""^(String.escaped s)^"\""
                else s
  | Procedure _ -> "#<runtime>"
  | Empty -> "()" ;;

(* Predictive, recursive descent parser for LL(7) grammar of Scheme. *)
type source = char Stream.t
let rec read (src : source) : expression =
  let syntax_error message =
    raise (Invalid_syntax (Stream.count src, message)) in

  let looking_at (s : string) : bool =
    List.for_all2 (fun a b -> Char.uppercase_ascii a = Char.uppercase_ascii b)
      (Stream.npeek (String.length s) src)
      (List.init (String.length s) (String.get s)) in

  let is_whitespace (c : char) : bool =
    match c with
    | ' ' | '\n' | '\r' | '\t' | '\x0C' -> true
    | _ -> false in

  let read_character () : expression =
    Stream.junk src;
    if looking_at "space" then
      Character ' '
    else if looking_at "newline" then
      Character '\n'
    else
      try Character (Stream.next src)
      with Stream.Failure ->
        syntax_error "expected a character after '#\\' but input ended instead" in

  let read_symbol () : string =
    let rec reader () : char Seq.node =
      match Stream.peek src with
      | None
        | Some '('
        | Some ')' -> Seq.Nil
      | Some c ->
         Stream.junk src;
         if is_whitespace c then Seq.Nil
         else Seq.Cons (c, reader) in
    String.of_seq reader in

  let read_string () : expression =
    let rec reader () : char Seq.node =
      match Stream.next src with
      | '"' -> Seq.Nil
      | '\\' ->
         let read_length (length : int) : string =
           let s = src
                   |> Stream.npeek 2
                   |> List.map (String.make 1)
                   |> String.concat "" in
           if String.length s = length then s
           else syntax_error "character code is not long enough" in
         Seq.Cons
           ((try
               match Stream.next src with
               | 'n' -> '\n'
               | 'r' -> '\r'
               | 't' -> '\t'
               | 'f' -> '\x0C'
               | 'x' -> Char.chr (int_of_string ("0x"^(read_length 2)))
               | '0' .. '9' -> Char.chr (int_of_string (read_length 3))
               | '\\' -> '\\'
               | '"' -> '"'
               | c -> syntax_error "not a valid escape character"
             with Failure _ -> syntax_error "not a valid character code"),
            reader)
      | c -> Seq.Cons (c, reader) in
    Stream.junk src;
    try String (String.of_seq reader)
    with Stream.Failure -> syntax_error "input ended before end of string" in

  let rec read_comment () : expression =
    try
      if Stream.next src = '\n' then read src
      else read_comment ()
    with Stream.Failure -> read src in

  let read_sequence () : expression list =
    let rec seq (after_dot : bool) () : expression Seq.node =
      match Stream.peek src with
      | None -> syntax_error "input ended before end of sequence"
      | Some ')' ->
         Stream.junk src;
         if after_dot then Seq.Nil
         else Seq.Cons (Empty, fun _ -> Seq.Nil)
      | Some '.' ->
         if after_dot then syntax_error "more than one expression after '.'"
         else if looking_at "..." then Seq.Cons (read src, seq false)
         else (Stream.junk src;
               Seq.Cons (read src, seq true))
      | Some c ->
         if is_whitespace c then
           (Stream.junk src;
            seq after_dot ())
         else
           if after_dot then syntax_error "more than one expression after '.'"
           else Seq.Cons (read src, seq false) in
    Stream.junk src;
    seq false
    |> List.of_seq
    |> List.rev in
  let read_vector () : expression =
    match read_sequence () with
    | Empty :: tail ->
       Vector (List.rev tail)
    | _ -> syntax_error "vector cannot contain '.'" in
  let read_list () : expression =
    let lst = read_sequence () in
    List.fold_left (fun acc el -> Pair (el, acc))
      (List.hd lst)
      (List.tl lst) in

  let number_of_string_opt (prefix : char) (s : string) : expression option =
    let to_parse = (if prefix = 'd' then ""
                    else "0"^(String.make 1 prefix))^s in
    match int_of_string_opt to_parse with
    | Some i -> Some (Integer i)
    | None ->
       match float_of_string_opt s with
       | Some f -> Some (Real f)
       | None -> None in
  let read_number (prefix : char) : expression =
    match number_of_string_opt prefix (read_symbol ()) with
    | Some exp -> exp
    | None -> syntax_error "invalid number" in

  let read_reader_construct () : expression =
    Stream.junk src;
    let exact (exp : expression) : expression =
      Stream.junk src;
      match exp with
      | Integer i -> Integer i
      | _ -> syntax_error
               ((write true exp)^" cannot be represented as an exact number") in
    let inexact (exp : expression) : expression =
      Stream.junk src;
      match exp with
      | Real f -> Real f
      | Integer i -> Real (float_of_int i)
      | _ -> syntax_error
               ((write true exp)^" cannot be represented as an inexact number") in
    match Stream.peek src with
    | None -> syntax_error "input ended before complete reader construct read"
    | Some '\\' -> read_character ()
    | Some '(' -> read_vector ()
    | Some 'e' -> exact (read src)
    | Some 'i' -> inexact (read src)
    | Some '<' -> syntax_error "not a valid reader construct"
    | Some prefix ->
       Stream.junk src;
       if looking_at "#e" then
         (Stream.junk src;
          exact (read_number prefix))
       else if looking_at "#f" then
         (Stream.junk src;
          inexact (read_number prefix))
       else read_number prefix in

  match Stream.peek src with
  | None -> raise Stream.Failure
  | Some '#' -> read_reader_construct ()
  | Some '(' -> read_list ()
  | Some ';' -> read_comment ()
  | Some '"' -> read_string ()
  | Some '\'' ->
     Stream.junk src;
     Pair (Symbol "quote", Pair (read src, Empty))
  | Some '`' ->
     Stream.junk src;
     Pair (Symbol "quasiquote", Pair (read src, Empty))
  | Some ',' ->
     Stream.junk src;
     let unquote =
       if Stream.peek src = Some '@' then "unquote-splicing"
       else "unquote" in
     Pair (Symbol unquote, Pair (read src, Empty))
  | Some c ->
     if is_whitespace c then
       (Stream.junk src;
        read src)
     else
       let symbol = read_symbol () in
       match number_of_string_opt 'd' symbol with
       | None -> Symbol symbol
       | Some exp -> exp ;;
