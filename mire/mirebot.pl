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
    % 1. Читаем приветствие
    read_line_to_codes(Stream, Welcome),
    format("DEBUG: Got welcome: ~s~n", [Welcome]),
    
    % 2. Отправляем имя
    format(Stream, "~s~n", [Name]),
    flush_output(Stream),
    
    % 3. Читаем ответ сервера (3 строки)
    read_line_to_codes(Stream, Resp1), % "Welcome, ..."
    read_line_to_codes(Stream, Resp2), % Описание комнаты
    read_line_to_codes(Stream, Resp3), % Health bar
    format("DEBUG: Responses: ~s, ~s, ~s~n", [Resp1, Resp2, Resp3]),
    
    % 4. Читаем приглашение "> "
    read_line_to_codes(Stream, Prompt),
    (member(0'>, Prompt) -> true ; fail).

% Упрощенный process/1
process(Stream) :-
    exit([Direction|_]),
    format(Stream, "move ~w~n", [Direction]),
    flush_output(Stream),
    retractall(exit(_)),
    
    % Читаем ответ (2 строки)
    read_line_to_codes(Stream, _Response),
    read_line_to_codes(Stream, _Prompt).

loop(Stream) :-
    catch(
        (read_line_to_codes(Stream, Codes),
        Error,
        (format('Error: ~w~n', [Error]), fail)
    ),
    (Codes == end_of_file ->
        format("Server disconnected~n", [])
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