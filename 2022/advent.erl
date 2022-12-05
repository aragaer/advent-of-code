-module(advent).
-export([get_int/0,read_all_lines/0]).

get_int() ->
    case io:get_line("") of
        eof -> eof;
        "\n" -> empty;
        Line ->
            {ok, [Num], _} = io_lib:fread("~u", string:chomp(Line)),
            Num
    end.

read_all_lines() -> read_all_lines([]).

read_all_lines(Ls) ->
    case io:get_line("") of
        eof -> lists:reverse(Ls);
        Line -> read_all_lines([string:chomp(Line)|Ls])
    end.
