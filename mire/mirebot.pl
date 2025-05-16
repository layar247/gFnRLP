:- use_module(library(socket)).

:- dynamic exit/1.
:- dynamic name_sent/0.  % Флаг, что имя отправлено

% Грамматика для разбора exits
e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].
exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).
parse_exits(Exits) --> [exits], exits(Exits), ['.'].

% Разбор токенов
parse(Tokens) :- 
    phrase(parse_exits(Exits), Tokens, Rest), 
    retractall(exit(_)), 
    assert(exit(Exits)).

parse(_).

% Фильтрация спецсимволов
filter_codes([], []).
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':']),
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :- 
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).

% Отправка имени при первом подключении
send_name(Stream) :-
    \+ name_sent,
    format(atom(Name), "bot~n", []),
    write(Stream, Name),
    flush_output(Stream),
    assertz(name_sent).

% Отправка движения, если есть exits
send_move(Stream) :-
    exit([Direction|_]),
    format(atom(Command), "move ~w~n", [Direction]),
    write(Stream, Command),
    flush_output(Stream),
    retractall(exit(_)).

% Главный цикл
loop(Stream) :-
    read_line_to_codes(Stream, Codes),
    (Codes == end_of_file -> 
        close(Stream)
    ; 
        filter_codes(Codes, Filtered),
        atom_codes(Atom, Filtered),
        tokenize_atom(Atom, Tokens),
        parse(Tokens),
        send_move(Stream),
        loop(Stream)
    ).

% Точка входа
main :-
    setup_call_cleanup(
        tcp_connect(localhost:3333, Stream, []),
        (send_name(Stream), loop(Stream)),
        close(Stream)).