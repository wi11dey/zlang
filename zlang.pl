:- use_module(library(dcg/basics)).

sexp(S) -->
    "(", !,
    sexps(S),
    ")".

sexp(A) -->
    symbol(A).

symbol(Symbol) -->
    string_without(`() \t\n\r`, Codes),
    { Codes \= [],
      atom_codes(Symbol, Codes)
    }.

sexps([S|Ss]) -->
    blanks,
    sexp(S),
    sexps(Ss).

sexps([]) --> blanks.
