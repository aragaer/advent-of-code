#!/usr/bin/env escript

evaluate(65,88) -> {4,3};
evaluate(65,89) -> {8,4};
evaluate(65,90) -> {3,8};
evaluate(66,88) -> {1,1};
evaluate(66,89) -> {5,5};
evaluate(66,90) -> {9,9};
evaluate(67,88) -> {7,2};
evaluate(67,89) -> {2,6};
evaluate(67,90) -> {6,7}.

get_round() ->
    case io:get_line("") of
        eof -> eof;
        Line ->
            [Other,_,Me|_] = Line,
            evaluate(Other,Me)
    end.

solve(Res1,Res2) ->
    case get_round() of
        eof -> io:format("~p~n~p~n", [Res1,Res2]);
        {V1,V2} -> solve(Res1+V1,Res2+V2)
    end.

main(_) -> solve(0,0).
