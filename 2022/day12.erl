#!/usr/bin/env escript

-module(day12).
-import(advent,[find_keys/2]).
-import(lists,[filtermap/2]).
-mode(compile).

can_move(From,To,Map) ->
    case maps:find(To,Map) of
        {ok, There} -> maps:get(From,Map) =< There+1;
        _ -> false
    end.

neighbours(Position,Map) ->
    filtermap(fun (D) ->
                      NP = advent:move(Position,D),
                      case can_move(Position,NP,Map) of
                          true -> {true,NP};
                          _ -> false
                      end
              end, [up,down,left,right]).

main(_) ->
    {RawData,_,_} = advent:lines_to_2d_map(advent:read_all_lines()),
    [Start,End] = lists:map(fun (K) -> hd(find_keys(K,RawData)) end, "SE"),
    Data = maps:merge(RawData, maps:from_list([{Start,$a},{End,$z}])),
    Explored = advent:bfs(End,Data,fun neighbours/2),
    Starts = find_keys($a,RawData),
    io:format("~p~n~p~n", [maps:get(Start,Explored),
                           lists:min(maps:values(maps:with(Starts,Explored)))]).
