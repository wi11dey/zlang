:- use_module(library(dcg/basics)).

sexp([quote,      X]) --> "'", !, sexp(X).
sexp([quasiquote, X]) --> "`", !, sexp(X).
sexp([unquote,    X]) --> ",", !, sexp(X).

sexp(S) --> "(", !, sexps(S), ")".
sexp(X) -->
    string_without(`() \t\n\r`, [C|Cs]),
    { catch(number_codes(X, [C|Cs]), _, atom_codes(X, [C|Cs])) }.

sexps([S|Ss]) --> blanks, sexp(S), sexps(Ss).
sexps([])     --> blanks.

zread(String, Sexp) :-
    string_codes(String, Codes),
    phrase(sexp(Sexp), Codes).

:- meta_predicate fmap(2, +, -).

fmap(F, Input, Output) :-
    call(F, Input, Output), !.
fmap(F, Input, Output) :-
    is_list(Input), !,
    maplist(fmap(F), Input, Output).
fmap(_, Input, Input).

desugar([quote, N], N) :- number(N).

:- dynamic expand/2.

zdefine([define, [quote, Name], Body]) :-
    assertz(expand(Name, Body)).
zdefine([define, [quasiquote, Name], Body]) :-
    assertz(expand(Name, Body)).
zdefine([define, Name|_]) :-
    !,
    zread(String, Name),
    format('Cannot define ~s~n', [String]),
    fail.

zeval([quote, Sexp], Sexp).

repl :-
    write('zlang> '),
    flush_output,
    read_line_to_string(user_input, Line),
    repl(Line).
repl(end_of_file) :- !,
    nl,
    writeln('Ta ta!').
repl(Line) :-
    (   zread(Line, Sexp)
    ->  fmap(desugar, Sexp, Desugared),
        writeln(Desugared)
    ;   writeln('Gibberish.')
    ),
    repl.

:- initialization(repl, main).
