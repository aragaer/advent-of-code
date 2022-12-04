#!/usr/bin/env escript

check(F1,T1,F2,T2) when F1 =< F2, T1 >= T2 -> 1;
check(F1,T1,F2,T2) when F2 =< F1, T2 >= T1 -> 1;
check(_,_,_,_) -> 0.

check2(F1,T1,F2,T2) when T1 >= F2, T2 >= F1 -> 1;
check2(_,_,_,_) -> 0.

solve(Res1,Res2) ->
    case io:get_line("") of
        eof -> io:format("~p~n~p~n", [Res1,Res2]);
        Line ->
            {ok, [F1,T1,F2,T2], _} = io_lib:fread("~u-~u,~u-~u", Line),
            solve(Res1+check(F1,T1,F2,T2),Res2+check2(F1,T1,F2,T2))
    end.

main(_) -> solve(0,0).
