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
        (retractall(exit(_)), assert(exit(Exits))
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
    % 1. Читаем приветствие "What is your name?"
    read_line_to_codes(Stream, _Welcome),
    
    % 2. Отправляем имя
    format(Stream, "~s~n", [Name]),
    flush_output(Stream),
    
    % 3. Читаем весь приветственный текст (3 строки)
    read_line_to_codes(Stream, _Line1), % "Welcome, Vlad!"
    read_line_to_codes(Stream, _Line2), % Описание комнаты
    read_line_to_codes(Stream, _Line3), % Health bar
    read_line_to_codes(Stream, _Prompt), % "> "
    
    % 4. Проверяем, было ли имя принято
    ( sub_atom(_Line1, _, _, _, "try again") ->
        login(Stream, Name) % повторяем
    ;
        true
    ).

% Улучшенный process/1
process(Stream) :-
    exit([Direction|_]),
    format(Stream, "move ~w~n", [Direction]),
    flush_output(Stream),
    retractall(exit(_)),
    
    % Читаем ответ сервера (2 строки)
    read_line_to_codes(Stream, _Response), % Результат движения
    read_line_to_codes(Stream, _Prompt).   % Новое приглашение "> "

loop(Stream) :-
    catch(
        (read_line_to_codes(Stream, Codes),
        Error,
        (format('Connection error: ~w~n', [Error]), fail)
    ),
    (Codes == end_of_file ->
        format("Server closed connection~n", [])
    ;
        filter_codes(Codes, Filtered),
        atom_codes(Atom, Filtered),
        tokenize_atom(Atom, Tokens),
        parse(Tokens),
        process(Stream),
        sleep(1),
        loop(Stream)
    ).

main :-
    retry_connect(3).

retry_connect(Attempts) :-
    Attempts > 0,
    catch(
        setup_call_cleanup(
            tcp_connect(localhost:3333, Stream, []),
            (
                login(Stream, "botname"), % Используем новую login/2
                loop(Stream)
            ),
            close(Stream)
        ),
        Error,
        (
            format('Attempt ~w failed: ~w~n', [Attempts, Error]),
            NewAttempts is Attempts - 1,
            sleep(5),
            retry_connect(NewAttempts)
        )
    ).