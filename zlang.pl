:- use_module(library(dcg/basics)).

sexp(S) --> "(", !, sexps(S), ")".
sexp(S) --> symbol(S).

sexp([quote,      X]) --> "'", !, sexp(X).
sexp([quasiquote, X]) --> "`", !, sexp(X).
sexp([unquote,    X]) --> ",", !, sexp(X).

symbol(S) -->
    string_without(`() \t\n\r`, [C|Cs]),
    { atom_codes(S, [C|Cs]) }.

sexps([S|Ss]) --> blanks, sexp(S), sexps(Ss).
sexps([])     --> blanks.
