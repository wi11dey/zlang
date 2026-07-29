:- use_module(library(dcg/basics)).

sexp([quote,      X]) --> "'", !, sexp(X).
sexp([quasiquote, X]) --> "`", !, sexp(X).
sexp([unquote,    X]) --> ",", !, sexp(X).

sexp(S) --> "(", !, sexps(S), ")".
sexp(X) -->
    string_without(`() \t\n\r`, [C|Cs]),
    { atom_codes(X, [C|Cs]) }.

sexps([S|Ss]) --> blanks, sexp(S), sexps(Ss).
sexps([])     --> blanks.

script([]) --> blanks, eos.
script([S|Ss]) --> blanks, sexp(S), script(Ss).

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

bind(Bindings, Name, Value) :- memberchk(Name-Value, Bindings).

pattern_match('_', _, []) :- !.
pattern_match([unquote, Name], Value, [Name-Value]) :- !,
    atom(Name).
pattern_match([P|Ps], [X|Xs], Bindings) :- !,
    pattern_match(P, X, Bs),
    pattern_match(Ps, Xs, Bss),
    append(Bs, Bss, Bindings).
pattern_match(X, X, []).
pattern_match(Pattern, Value, Bindings) :-
    expand(Value, Expanded),
    pattern_match(Pattern, Expanded, Bindings).

:- dynamic expand/2.

expand(S, _) :- atom(S), !, fail.

zdefine([define, [quote, Name], Body]) :-
    asserta((expand(Name, Body))).
zdefine([define, [quasiquote, Pattern], Body]) :-
    assertz((expand(S, Output) :- pattern_match(Pattern, S, Bindings), fmap(bind(Bindings), Body, Output))).
zdefine([define|_]) :- !,
    writeln('syntax error'),
    fail.

%% zeval([], []).
zeval(A, A) :- atom(A), \+ expand(A, _).
zeval(S, Fixpoint) :-
    expand(S, Expanded),
    zeval(Expanded, Fixpoint).

repl :-
    write('zlang> '),
    flush_output,
    read_line_to_string(user_input, Line),
    repl(Line).
repl(end_of_file) :- !, nl.
repl(Line) :-
    zread(Line, Sexp), !,
    (zdefine(Sexp);
     zeval(Sexp, Result) -> writeln(Result);
     writeln('semantic error')),
    repl.
repl(_) :-
    writeln('syntax error'),
    repl.

main([]) :- repl.
main([Script]) :-
    read_file_to_codes(Script, Codes, []),
    phrase(script(Sexps), Codes),
    append(Definitions, [Return], Sexps),
    maplist(zdefine, Definitions),
    zeval(Return, Result),
    writeln(Result).

:- initialization(main, main).
