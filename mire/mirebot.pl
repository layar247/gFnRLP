:- use_module(library(socket)).

:- dynamic exit/1.

e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].

exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).

parse_exits(Exits) --> [exits], exits(Exits), ['.'].

parse(Tokens) :-
    phrase(parse_exits(Exits), Tokens, _Rest),
    retractall(exit(_)),
    assert(exit(Exits)).

parse(_).

filter_codes([], []).
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':']),
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).

process(Stream) :-
    exit([Direction|_]),
    format(atom(Command), 'move ~w~n', [Direction]),
    write(Command),
    write(Stream, Command),
    flush_output(Stream),
    retractall(exit(_)).

process(_).

loop(Stream) :-
    read_line_to_codes(Stream, Codes),
    filter_codes(Codes, Filtered),
    atom_codes(Atom, Filtered),
    tokenize_atom(Atom, Tokens),
    write(Tokens),
    nl,
    parse(Tokens),
    flush(),
    sleep(1),
    process(Stream),
    loop(Stream).

login(Stream, Name) :-
    read_line_to_codes(Stream, Codes),
    atom_codes(Prompt, Codes),
    format('Server prompt: ~s~n', [Prompt]),  % выводит приглашение сервера
    format(Stream, "~s~n", [Name]),          % отправляет имя (логин) на сервер
    flush_output(Stream).

main :-
    setup_call_cleanup(
        tcp_connect(localhost:3333, Stream, []),
        (
            login(Stream, "botname"),  % отправляем имя
            loop(Stream)               % запускаем основной игровой цикл
        ),
        close(Stream)
    ).
