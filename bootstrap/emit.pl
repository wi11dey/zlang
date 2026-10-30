:- ensure_loaded('../zlang.pl').
:- use_module(library(utf8)).

% An output observer, not another evaluator. All rewrites use zlang.pl.
% The four constructors describe a lazy tree of bytes; no LLVM is known here.
emit_tree([out_text, Atom]) :- !,
    atom(Atom), atom_codes(Atom, Codes),
    phrase(utf8_codes(Codes), Bytes), maplist(put_byte, Bytes).
emit_tree([out_char, Code]) :- !,
    atom(Code), atom_codes(Code, [Digit|Digits]),
    maplist(between(0'0, 0'9), [Digit|Digits]),
    number_codes(N, [Digit|Digits]), between(0, 255, N), put_byte(N).
emit_tree([out_empty]) :- !.
emit_tree([out_cat, Left, Right]) :- !,
    emit_tree(Left), emit_tree(Right).
emit_tree(Expression) :-
    once(expand(Expression, Expanded)),
    emit_tree(Expanded).

emit_file(Path) :-
    read_file_to_codes(Path, Codes, []),
    phrase(script(Expressions), Codes),
    append(Definitions, [Result], Expressions),
    maplist(zdefine, Definitions),
    emit_tree(Result).

emit_main :-
    current_prolog_flag(argv, [Path]),
    set_stream(user_output, type(binary)),
    (emit_file(Path) -> true ; throw(error(bootstrap_output_failed, Path))).
