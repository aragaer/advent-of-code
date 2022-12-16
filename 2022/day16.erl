#!/usr/bin/env escript

-module(day16).
-mode(compile).

parse_one_line(Line) ->
    {L1,L2} = lists:split(9,Line),
    Name = string:substr(L1,7,2),
    {ok,[Rate],L3} = io_lib:fread("has flow rate=~u;", L2),
    Links = string:substr(L3,24),
    {Name,Rate,string:lexemes(Links,", ")}.

distances_from(Room,Rooms,Links) ->
    FromRoom = advent:bfs(Room,Links,fun maps:get/2),
    maps:from_list(lists:map(fun (R) -> {{Room,R},maps:get(R,FromRoom)} end, Rooms)).

search([],Values,_,_) -> Values;
search([{T,Value,Actions}|Queue],Values,Connectivity,Valves) ->
    Last = if Actions == [] -> "AA";
              true -> hd(Actions)
           end,
    New = lists:filtermap(fun (L) ->
                                  D = maps:get({Last,L},Connectivity),
                                  if T > D + 1 ->
                                          TL = T-D-1,
                                          E = maps:get(L,Valves) * TL,
                                          {true, {TL,E+Value,[L|Actions]}};
                                     true -> false
                                  end
                           end, maps:keys(Valves) -- Actions),
    search(lists:append(Queue,New),
           lists:foldl(fun ({_,NV,A},V) ->
                               Key = lists:sort(A),
                               {OldValue,_} = maps:get(Key,V,{0,nil}),
                               if OldValue > NV -> V;
                                  true -> maps:put(Key,{NV,A},V)
                               end
                       end, Values, New),
           Connectivity,Valves).

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
    X = search([{30,0,[]}],maps:new(),Connectivity,Valves),
    {Res1,A11} = lists:max(maps:values(X)),
    io:format("~p", [Res1]),
    if Args == [] -> io:format("~n");
       true -> io:format(" ~p~n", [lists:reverse(A11)])
    end,
    Y = search([{26,0,[]}],maps:new(),Connectivity,Valves),
    {Res2,A21,A22} = lists:max([{V1+V2,A1,A2} || {K1,{V1,A1}} <- maps:to_list(Y),
                                                 {K2,{V2,A2}} <- maps:to_list(Y),
                                                 K1 -- K2 == K1]),
    io:format("~p", [Res2]),
    if Args == [] -> io:format("~n");
       true -> io:format(" ~p ~p~n", [lists:reverse(A21),lists:reverse(A22)])
    end.
