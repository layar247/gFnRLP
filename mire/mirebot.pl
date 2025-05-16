:- use_module(library(socket)).
:- dynamic exit/1.

% DCG для разбора направлений
e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].
exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).
parse_exits(Exits) --> [exits], exits(Exits), ['.'].

% Парсер для извлечения выходов
parse(Tokens) :-
    ( phrase(parse_exits(Exits), Tokens, _) ->
        retractall(exit(_)),
        assert(exit(Exits))
    ; true ).

% Фильтрация лишних символов и приведение к нижнему регистру
filter_codes([], []).
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':']),
    !,
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).

% Отправка команды движения, если есть доступный выход
process(Stream) :-
    exit([Direction|_]),
    format(Stream, "move ~w~n", [Direction]),
    flush_output(Stream),
    retractall(exit(_)).
process(_).

% Чтение строк от сервера, парсинг, отправка команд и рекурсивный вызов
loop(Stream) :-
    catch(
        read_line_to_codes(Stream, Codes),
        Error,
        ( format('Connection error: ~w~n', [Error]), fail )
    ),
    ( Codes == end_of_file ->
        format("Server closed connection~n", [])
    ;
        filter_codes(Codes, Filtered),
        atom_codes(Atom, Filtered),
        tokenize_atom(Atom, Tokens),
        format('Server: ~s~n', [Atom]),  % Для отладки выводим строку
        parse(Tokens),
        process(Stream),
        sleep(1),
        loop(Stream)
    ).

% Логин: читаем приглашение, запрашиваем имя у пользователя и отправляем серверу
login(Stream) :-
    read_line_to_codes(Stream, Codes),
    atom_codes(Prompt, Codes),
    format('Server prompt: ~s~n', [Prompt]),

    write('Enter your name: '), flush_output,
    read_line_to_codes(user_input, NameCodes),
    atom_codes(Name, NameCodes),

    format(Stream, "~s~n", [Name]),
    flush_output(Stream).

% Главная точка входа
main :-
    setup_call_cleanup(
        tcp_connect(localhost:3333, Stream, []),
        (
            login(Stream),
            loop(Stream)
        ),
        close(Stream)
    ).
