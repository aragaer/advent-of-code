#!/usr/bin/env escript

-import(lists,[sort/1,last/1,sum/1]).
-import(advent,[get_int/0]).

result(X) -> io:format("~p~n~p~n", [last(X),sum(X)]).

get_max(L, V) ->
    [_|R] = sort([V|L]),
    R.

solve(Max, Current) ->
    case get_int() of
        eof -> result(get_max(Max, Current));
        empty -> solve(get_max(Max, Current), 0);
        Num -> solve(Max, Current+Num)
    end.

main(_) -> solve([0, 0, 0], 0).
