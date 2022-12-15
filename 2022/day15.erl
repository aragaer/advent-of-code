#!/usr/bin/env escript

-module(day15).
-mode(compile).

distance({X1,Y1},{X2,Y2}) -> abs(X1-X2)+abs(Y1-Y2);
distance({_,Y1},Y2) -> abs(Y1-Y2).

overlap({SX,SY,BX,BY},Y) ->
    Range = distance({SX,SY},{BX,BY}),
    Distance = distance({SX,SY},Y),
    if Range >= Distance -> {true, {SX-(Range-Distance),SX+(Range-Distance)}};
       true -> false
    end.

range_overlap({F1,T1},{F2,T2}) when T1 >= F2, T2 >= F1 -> true;
range_overlap(_,_) -> false.

intersect({F1,T1},{F2,T2}) ->
    case range_overlap({F1,T1},{F2,T2}) of
        false -> false;
        true -> {true, {max(F1,F2),min(T1,T2)}}
    end.

parse_sensor([]) -> false;
parse_sensor(Line) ->
    {ok, R, _} = io_lib:fread("Sensor at x=~d, y=~d: closest beacon is at x=~d, y=~d", Line),
    {true, list_to_tuple(R)}.

add_range(Range,Ranges) ->
    {Overlaps,NonOverlaps} = lists:partition(fun (R) -> range_overlap(R,Range) end, Ranges),
    Group = [Range|Overlaps],
    F = lists:min(lists:map(fun ({F,_}) -> F end, Group)),
    T = lists:max(lists:map(fun ({_,T}) -> T end, Group)),
    [{F,T}|NonOverlaps].

project(Sensors,Row,LimFun) ->
    Ranges = lists:filtermap(fun (S) -> overlap(S,Row) end, Sensors),
    LimRanges = if LimFun == nil -> Ranges;
                   true -> lists:filtermap(LimFun, Ranges)
                end,
    lists:foldl(fun add_range/2, [], LimRanges).

part2(Sensors,Lim,N) ->
    Ranges = project(Sensors,N,fun (R) -> intersect(R,{0,Lim}) end),
    case Ranges of
        [{0,Lim}] -> part2(Sensors,Lim,N+1);
        [{_,T1},{_,T2}] -> (min(T1,T2)+1) * 4000000 + N
    end.

main(_) ->
    Sensors = lists:filtermap(fun parse_sensor/1, advent:read_all_lines()),
    TheRow = case hd(Sensors) of
                 {2,18,-2,15} -> 10;
                 _ -> 2000000
             end,
    TheSearch = TheRow * 2,
    TheRowOverlaps = project(Sensors,TheRow,nil),
    Part1 = lists:foldl(fun ({F,T},A) -> A+T-F end, 0, TheRowOverlaps),
    io:format("~p~n~p~n",[Part1,part2(Sensors,TheSearch,0)]).
