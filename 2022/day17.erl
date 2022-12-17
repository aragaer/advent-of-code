#!/usr/bin/env escript

-module(day17).
-import(lists,[enumerate/1,flatmap/2,filtermap/2]).
-mode(compile).

get_next_rock(Idx,Height) ->
    [{_,Rock}] = ets:lookup(rocks, Idx rem 5 + 1),
    lists:map(fun ({X,Y}) -> {X+2, Y+Height+3} end, Rock).

get_wind(Wind,Idx) -> lists:nth(Idx rem length(Wind)+1, Wind).

move_rock(Rock,Dir) -> lists:map(fun (C) -> advent:move(C,Dir) end, Rock).

rock_check({X,_},_) when X =< 0 -> false;
rock_check({X,_},_) when X > 7 -> false;
rock_check({_,Y},_) when Y =< 0 -> false;
rock_check(C,M) -> not maps:is_key(C,M).

try_move_rock(Rock,Map,Dir) ->
    Rock1 = move_rock(Rock,Dir),
    case lists:all(fun (C) -> rock_check(C,Map) end, Rock1) of
        true -> {true, Rock1};
        _ -> {false, Rock}
    end.

iterate_rock(Rock,Map,Wind,Idx) ->
    {_,AfterWind} = try_move_rock(Rock,Map,get_wind(Wind,Idx)),
    case try_move_rock(AfterWind,Map,down) of
        {true,R} -> iterate_rock(R,Map,Wind,Idx+1);
        _ ->
            RockTop = lists:max(lists:map(fun ({_,Y}) -> Y end, Rock)),
            {maps:merge(maps:from_keys(AfterWind,$#), Map),RockTop,Idx+1}
    end.

drop_rock(Map,Height,Wind,WindIdx,RockIdx) ->
    iterate_rock(get_next_rock(RockIdx,Height),Map,Wind,WindIdx).

line2num(Map,Y) ->
    Line = lists:map(fun (I) -> maps:is_key({I,Y},Map) end, lists:seq(1,7)),
    case ets:lookup(signature, Line) of
        [{_,Val}] -> Val;
        _ ->
            Val = lists:foldl(fun (true,A) -> A*2+1;
                                  (false,A) -> A*2
                              end, 0, Line),
            ets:insert(signature,{Line,Val}),
            Val
    end.

find_loop(Map,Height,Wind,WindIdx,RockIdx,Seen) ->
    {NMap,NH,NWI} = drop_rock(Map,Height,Wind,WindIdx,RockIdx),
    H = max(NH,Height),
    Sig = lists:map(fun (Y) -> line2num(NMap,Y) end, lists:seq(H-100,H)),
    Key = {NWI rem length(Wind),RockIdx rem 5,Sig},
    case maps:find(Key,Seen) of
        {ok,{PI,PH}} -> {PI,RockIdx+1-PI,H-PH,Seen};
        _ -> find_loop(NMap,H,Wind,NWI,RockIdx+1,
                       maps:put(Key,{RockIdx+1,H},Seen))
    end.

find_loop(Wind) ->
    ets:new(signature, [public, named_table]),
    find_loop(maps:new(),0,Wind,0,0,maps:new()).

result(Steps,{Start,Len,Height,Cache}) ->
    Count = (Steps - Start) div Len,
    TailLen = (Steps - Start) rem Len,
    {_,Tail} = lists:keyfind(Start+TailLen,1,maps:values(Cache)),
    Count * Height + Tail.

map2rock(RockMap) ->
    flatmap(fun ({Y,L}) ->
                    filtermap(fun ({X,$#}) -> {true, {X,Y}};
                                  (_) -> false
                              end, enumerate(L))
            end, enumerate(lists:reverse(string:lexemes(RockMap,"\n")))).

main(_) ->
    ets:new(rocks, [public, named_table]),
    Rocks = [["####"],
             [".#.\n###\n.#."],
             ["..#\n..#\n###"],
             ["#\n#\n#\n#"],
             ["##\n##"]],
    lists:foreach(fun ({I,RockMap}) ->
                          ets:insert(rocks, {I, map2rock(RockMap)})
                  end, enumerate(Rocks)),
    Wind = lists:map(fun ($>) -> right;
                         ($<) -> left
                     end, string:chomp(io:get_line(""))),
    LoopData = find_loop(Wind),
    io:format("~p~n~p~n", [result(2022,LoopData),
                           result(1000000000000,LoopData)]).
