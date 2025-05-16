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
    (phrase(parse_exits(Exits), Tokens, _Rest) ->
        (retractall(exit(_)), assert(exit(Exits)))
    ;
        true.

filter_codes([], []) :- !.
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':']),
    !,
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).

login(Stream, Name) :-
    % Читаем приветствие
    read_line_to_codes(Stream, Welcome),
    format("DEBUG: Welcome: ~s~n", [Welcome]),
    
    % Отправляем имя
    format(Stream, "~s~n", [Name]),
    flush_output(Stream),
    
    % Читаем ответ сервера
    read_line_to_codes(Stream, Response1),
    read_line_to_codes(Stream, Response2),
    read_line_to_codes(Stream, Response3),
    format("DEBUG: Responses: ~s, ~s, ~s~n", [Response1, Response2, Response3]),
    
    % Проверяем, было ли имя принято
    ( member(0't, Response1), member(0'r, Response1), member(0'y, Response1) ->
        login(Stream, Name)
    ;
        true
    ).

process(Stream) :-
    exit([Direction|_]),
    format(Stream, "move ~w~n", [Direction]),
    flush_output(Stream),
    retractall(exit(_)),
    
    % Читаем ответ сервера
    read_line_to_codes(Stream, Response),
    read_line_to_codes(Stream, Prompt),
    format("DEBUG: Move response: ~s~n", [Response]).

loop(Stream) :-
    catch(
        read_line_to_codes(Stream, Codes),
        Error,
        (format('Connection error: ~w~n', [Error]), fail)
    ),
    (Codes == end_of_file ->
        format("Server closed connection~n", [])
    ;
        format("DEBUG: Received: ~s~n", [Codes]),
        filter_codes(Codes, Filtered),
        atom_codes(Atom, Filtered),
        tokenize_atom(Atom, Tokens),
        parse(Tokens),
        process(Stream),
        sleep(1),
        loop(Stream)
    ).

main :-
    setup_call_cleanup(
        tcp_connect(localhost:3333, Stream, []),
        (
            login(Stream, "botname"),
            loop(Stream)
        ),
        close(Stream)
    ).