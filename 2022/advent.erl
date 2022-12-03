-module(advent).
-export([get_int/0]).

get_int() ->
    case io:get_line("") of
        eof -> eof;
        "\n" -> empty;
        Line ->
            {ok, [Num], _} = io_lib:fread("~u", string:chomp(Line)),
            Num
    end.
