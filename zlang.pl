:- use_module(library(dcg/basics)).

sexp([quote,      X]) --> "'", !, sexp(X).
sexp([quasiquote, X]) --> "`", !, sexp(X).
sexp([unquote,    X]) --> ",", !, sexp(X).

sexp(S) --> "(", !, sexps(S), ")".
sexp(S) --> symbol(S).

symbol(S) -->
    string_without(`() \t\n\r`, [C|Cs]),
    { atom_codes(S, [C|Cs]) }.

sexps([S|Ss]) --> blanks, sexp(S), sexps(Ss).
sexps([])     --> blanks.

zread(String, Sexp) :- string_codes(String, Codes), phrase(sexp(Sexp), Codes).

repl :-
    write('zlang> '),
    flush_output,
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  nl, writeln('Ta ta!')
    ;   (   zread(Line, Sexp)
        ->  writeln(Sexp)
        ;   writeln('Gibberish.')
        ),
        repl
    ).

:- initialization(repl, main).
