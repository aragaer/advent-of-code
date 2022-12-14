#!/usr/bin/env escript

-module(day14).
-import(lists,[dropwhile/2,map/2]).
-mode(compile).

draw_line(L,M) ->
    draw_segment(map(fun (P) ->
                             {ok, [X,Y], _} = io_lib:fread("~u,~u",P),
                             {X,Y}
                     end, string:lexemes(L," ->")),M).

draw_segment([],M) -> M;
draw_segment([C],M) -> maps:put(C,$#,M);
draw_segment([C1,C2|R],M) ->
    draw_segment([C2|R],
                 maps:merge(M,maps:from_keys(line_between(C1,C2),$#))).

line_between({X1,Y1},{X2,Y2}) when X1 == X2 ->
    map(fun (Y) -> {X1,Y} end, lists:seq(min(Y1,Y2),max(Y1,Y2)));
line_between({X1,Y1},{X2,Y2}) when Y1 == Y2 ->
    map(fun (X) -> {X,Y1} end, lists:seq(min(X1,X2),max(X1,X2))).

drop_sand(_,[Min,_],{X,_}) when X < Min -> abyss;
drop_sand(_,[_,Max],{X,_}) when X > Max -> abyss;
drop_sand(Map,Bottom,{X,Y}) when Y == Bottom -> maps:put({X,Y},$o,Map);
drop_sand(Map,Edges,Sand) ->
    Next = dropwhile(fun (C) -> maps:is_key(C,Map) end,
                     map(advent:move(Sand), [up,upleft,upright])),
    case Next of
        [] -> maps:put(Sand,$o,Map);
        [C|_] -> drop_sand(Map,Edges,C)
    end.

pour_sand(Map,Edges,Count) ->
    case drop_sand(Map,Edges,{500,0}) of
        abyss -> Count;
        NewMap ->
            Done = maps:is_key({500,0}, NewMap),
            if Done -> Count+1;
               true -> pour_sand(NewMap,Edges,Count+1)
            end
    end.

main(_) ->
    Lines = advent:read_all_lines(),
    Map = lists:foldl(fun draw_line/2, maps:new(), Lines),
    Xs = map(fun ({X,_}) -> X end, maps:keys(Map)),
    Ys = map(fun ({_,Y}) -> Y end, maps:keys(Map)),
    io:format("~p~n~p~n", [pour_sand(Map,[lists:min(Xs),lists:max(Xs)],0),
                           pour_sand(Map,lists:max(Ys)+1,0)]).
