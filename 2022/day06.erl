#!/usr/bin/env escript

solve(N,L,Line) ->
    {F4,_} = lists:split(L,Line),
    case length(lists:uniq(F4)) of
        L -> N+L;
        _ -> solve(N+1,L,tl(Line))
    end.

main(_) ->
    case io:get_line("") of
        eof -> done;
        Line ->
            io:format("~p ~p~n", [solve(0,4,Line),
                                  solve(0,14,Line)]),
            main(next)
    end.
