#!/usr/bin/env escript

get_code(X) when X < 91 -> X - 65 + 27;
get_code(X) -> X - 97 + 1.

evaluate(Line) -> get_common(tuple_to_list(lists:split(length(Line) div 2, Line))).

evaluate2([L1,L2],L3) -> {get_common([L1,L2,L3]),[]};
evaluate2(Ls,L) -> {0,[L|Ls]}.

get_common(Ls) ->
    [X|_] = sets:to_list(sets:intersection(lists:map(fun sets:from_list/1, Ls))),
    get_code(X).

solve(Res1,Res2,Lst) ->
    case io:get_line("") of
        eof -> io:format("~p~n~p~n", [Res1,Res2]);
        L ->
            Line = string:chomp(L),
            {Eval2,Lst2} = evaluate2(Lst,Line),
            solve(Res1+evaluate(Line), Res2+Eval2, Lst2)
    end.

main(_) -> solve(0,0,[]).
