:- use_module(library(dcg/basics)).

sexp([quote,      X]) --> "'", !, sexp(X).
sexp([quasiquote, X]) --> "`", !, sexp(X).
sexp([unquote,    X]) --> ",", !, sexp(X).

sexp(S) --> "(", !, sexps(S), ")".
sexp(N) --> numeric(N).
sexp(S) --> symbol(S).

numeric(N) -->
    string_without(`() \t\n\r`, [C|Cs]),
    { catch(number_codes(N, [C|Cs]), _, fail) }.

symbol(S) -->
    string_without(`() \t\n\r`, [C|Cs]),
    { atom_codes(S, [C|Cs]) }.

sexps([S|Ss]) --> blanks, sexp(S), sexps(Ss).
sexps([])     --> blanks.

zread(String, Sexp) :- string_codes(String, Codes), phrase(sexp(Sexp), Codes).

:- dynamic expand/2.

zdefine([define, [quote, Name], Body]) :-
    assertz(expand(Name, Body)).
zdefine([define, [quasiquote, Name], Body]) :-
    writeln('expand(', Name, ', Result).').
zdefine([define, Name|_]) :-
    !,
    zread(String, Name),
    writeln('Cannot define', String),
    fail.

zeval([quote, Sexp], Sexp).

repl :-
    write('zlang> '),
    flush_output,
    read_line_to_string(user_input, Line),
    (Line == end_of_file -> nl, writeln('Ta ta!');
     (zread(Line, Sexp) -> writeln(Sexp); 
      writeln('Gibberish.')),
     repl).

:- initialization(repl, main).
