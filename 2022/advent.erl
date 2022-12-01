-module(advent).
-export([get_int/0]).

get_int() ->
    case io:get_line("") of
        eof -> eof;
        "\n" -> empty;
        Line ->
            LineWithoutNL = string:strip(string:strip(Line, both, 13), both, 10),
            {ok, [Num], _} = io_lib:fread("~u", LineWithoutNL),
            Num
    end.
