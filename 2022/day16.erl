#!/usr/bin/env escript

-module(day16).
-mode(compile).

parse_one_line(Line) ->
    {L1,L2} = lists:split(9,Line),
    Name = string:substr(L1,7,2),
    {ok,[Rate],L3} = io_lib:fread("has flow rate=~u;", L2),
    Links = string:substr(L3,24),
    {Name,Rate,string:lexemes(Links,", ")}.

solve([],_,_,Res) -> Res;
solve(_,[],_,Res) -> Res;
solve(Rooms,[{T,L}|As],{Connectivity,Valves},{Res,A}) ->
    {Ro,Ao} = solve(Rooms,As,{Connectivity,Valves},{Res,A}),
    {Rt,At} = lists:foldl(fun ({R,D},{B,BA}) ->
                                  NT = T-D-1,
                                  E = maps:get(R,Valves),
                                  {R1,A1} = solve(Rooms--[R],[{NT,R}|As],
                                                  {Connectivity,Valves},
                                                  {Res+E*NT,[{R,NT,length(As)}|A]}),
                                  if R1 > B -> {R1,A1};
                                     true -> {B,BA}
                                  end
                          end, {Res,A},
                          lists:filtermap(fun (R) ->
                                                  D = maps:get({L,R},Connectivity),
                                                  if D < T -> {true,{R,D}};
                                                     true -> false
                                                  end
                                          end, Rooms)),
    if Ro > Rt -> {Ro,Ao};
       true -> {Rt,At}
    end.

distances_from(Room,Rooms,Links) ->
    FromRoom = advent:bfs(Room,Links,fun maps:get/2),
    maps:from_list(lists:map(fun (R) -> {{Room,R},maps:get(R,FromRoom)} end, Rooms)).

print_log(Actions,Time) ->
    lists:foreach(fun ({R,T,A}) ->
                          io:format("Minute ~p ~s open valve ~s~n",
                                    [Time-T, lists:nth(A+1, ["I","Elephant"]), R])
                  end, lists:sort(fun ({_,T1,_},{_,T2,_}) -> T1 > T2 end, Actions)).

main(Args) ->
    {Rooms,Links} = lists:foldl(fun ([],A) -> A;
                                    (L,{Vs,Ls}) ->
                                        {N,R,Ns} = parse_one_line(L),
                                        {maps:put(N,R,Vs),maps:put(N,Ns,Ls)}
                                end, {maps:new(),maps:new()}, advent:read_all_lines()),
    Valves = maps:filter(fun (_,V) -> V > 0 end, Rooms),
    VRooms = maps:keys(Valves),
    Connectivity = lists:foldl(fun (R,M) ->
                                       maps:merge(M,distances_from(R,VRooms,Links))
                               end,
                               distances_from("AA",VRooms,Links),
                               VRooms),
    {Res1,Actions1} = solve(VRooms,[{30,"AA"}],{Connectivity,Valves},{0,[]}),
    {Res2,Actions2} = solve(VRooms,[{26,"AA"},{26,"AA"}],{Connectivity,Valves},{0,[]}),
    io:format("~p~n", [Res1]),
    if Args == [] -> nothing;
       true -> print_log(Actions1,30)
    end,
    io:format("~p~n", [Res2]),
    if Args == [] -> nothing;
       true -> print_log(Actions2,26)
    end.
