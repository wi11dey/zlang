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

bind(Bindings, [unquote, Name], Value) :- memberchk(Name-Value, Bindings).

pattern_match('_', _, []) :- !.
pattern_match([unquote, Name], Value, [Name-Value]) :- !,
    atom(Name).
pattern_match([P|Ps], [X|Xs], Bindings) :-
    pattern_match(P, X, Bs),
    pattern_match(Ps, Xs, Bss),
    append(Bs, Bss, Bindings).
pattern_match(X, X, []).

desugar([quote, N], N) :- number(N).

:- dynamic expand/2.

zdefine([define, [quote, Name], Body]) :-
    assertz(expand(Name, Body)),
    writeln('Roger that.').
zdefine([define, [quasiquote, Pattern], Body]) :-
    assertz((expand(S, Output) :- pattern_match(Pattern, S, Bindings), fmap(bind(Bindings), Body, Output))).
zdefine([define, Name|_]) :- !,
    format('Cannot define ~s~n', [Name]),
    fail.

zeval([], []).
zeval(N, N) :- number(N).
zeval(S, Fixpoint) :-
    expand(S, Expanded), !,
    zeval(Expanded, Fixpoint).

repl :-
    write('zlang> '),
    flush_output,
    read_line_to_string(user_input, Line),
    repl(Line).
repl(end_of_file) :- !,
    nl,
    writeln('Ta ta!').
repl(Line) :-
    zread(Line, Sexp), !,
    fmap(desugar, Sexp, Desugared),
    (zdefine(Desugared) -> true;
     zeval(Desugared, Result) -> writeln(Result);
     writeln('_|_')),
    repl.
repl(_) :-
    writeln('Gibberish.'),
    repl.

:- initialization(repl, main).
