:- ensure_loaded('../../zlang.pl').

% Reuse the actual implementation, including its cuts and choice points.
oracle_main :-
    current_prolog_flag(argv, [Path]),
    read_file_to_codes(Path, Codes, []),
    phrase(script(Expressions), Codes),
    append(Definitions, [Result], Expressions),
    maplist(zdefine, Definitions),
    once(zeval(Result, Atom)),
    write(Atom), nl.
