:- use_module(library(dcg/basics)).

sexp(S) -->
    "(", !,
    sexps(S),
    ")".

sexp(A) -->
    symbol(A).

sexps([S|Ss]) -->
    blanks,
    sexp(S),
    sexps(Ss).

sexps([]) --> blanks.
