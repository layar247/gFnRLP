:- use_module(library(socket)).
:- dynamic exit/1.

% DCG правила остаются без изменений
e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].
exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).
parse_exits(Exits) --> [exits], exits(Exits), ['.'].

% Улучшенный парсер с обработкой ошибок
parse(Tokens) :-
    (phrase(parse_exits(Exits), Tokens, _Rest) ->
        (retractall(exit(_)), assert(exit(Exits))
    ;
        true.

% Фильтр с защитой от пустого ввода
filter_codes([], []) :- !.
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':']),
    !,
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).

% Обработчик команд с защитой от сбоев
process(Stream) :-
    (exit([Direction|_]) ->
        format(atom(Command), 'move ~w~n', [Direction]),
        format(Stream, '~s', [Command]),
        flush_output(Stream),
        retractall(exit(_))
    ;
        true.

% Улучшенный главный цикл
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

% Инициализация с переподключением
main :-
    retry_connect(3).

retry_connect(Attempts) :-
    Attempts > 0,
    catch(
        setup_call_cleanup(
            tcp_connect(localhost:3333, Stream, []),
            (
                login(Stream, "botname"),
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