#!/usr/bin/env escript

-mode(compile).

parse(LineNum,Data) ->
    case io:get_line("") of
        eof -> {Data,LineNum};
        Line -> parse(LineNum+1,
                      lists:foldl(fun ({I,C},D) -> maps:put({I-1,LineNum},C-$0,D) end,
                                  Data, lists:enumerate(string:chomp(Line))))
    end.

get_width(Map,W) ->
    case maps:find({0,W},Map) of
        error -> W;
        _ -> get_width(Map,W+1)
    end.

ind_u(X,Y,_,_) -> lists:map(fun (I) -> {X,I} end, lists:seq(Y-1,0,-1)).
ind_d(X,Y,_,H) -> lists:map(fun (I) -> {X,I} end, lists:seq(Y+1,H-1)).
ind_l(X,Y,_,_) -> lists:map(fun (I) -> {I,Y} end, lists:seq(X-1,0,-1)).
ind_r(X,Y,W,_) -> lists:map(fun (I) -> {I,Y} end, lists:seq(X+1,W-1)).

ind(X,Y,W,H) -> [ind_u(X,Y,W,H),ind_d(X,Y,W,H),ind_l(X,Y,W,H),ind_r(X,Y,W,H)].

get_from(Map) -> fun (I) -> maps:get(I,Map) end.
get_heights(Map, Is) -> lists:map(get_from(Map), Is).

get_min_max(Map,X,Y,W,H) ->
    Is = ind(X,Y,W,H),
    lists:min(lists:map(fun ([]) -> -1;
                            (L) -> lists:max(get_heights(Map, L))
                        end, Is)).

is_visible(Map,X,Y,W,H) ->
    maps:get({X,Y},Map) > get_min_max(Map,X,Y,W,H).

part1(Map,W,H) ->
    lists:sum(lists:map(fun (Y) ->
                                length(lists:filter(fun (X) -> is_visible(Map,X,Y,W,H) end,
                                                    lists:seq(1,W-2))) end,
                        lists:seq(1,H-2))) + W * 2 + H * 2 - 4.

visible(_,[]) -> 0;
visible(V0,[V|Vs]) when V0 > V -> 1 + visible(V0,Vs);
visible(_,_) -> 1.

scenic(Map,X,Y,W,H) ->
    V = maps:get({X,Y},Map),
    Is = ind(X,Y,W,H),
    [U,D,L,R] = lists:map(fun (Is1) -> visible(V,get_heights(Map,Is1)) end, Is),
    U*D*L*R.

part2(Map,W,H) -> lists:max([scenic(Map,X,Y,W,H) || X <- lists:seq(0,W-1), Y <- lists:seq(0,H-1)]).

main(_) ->
    {Map,Height} = parse(0,maps:new()),
    Width = get_width(Map,0),
    io:format("~p~n~p~n", [part1(Map,Width,Height),part2(Map,Width,Height)]).

