#!/usr/bin/env escript

-module(day19).
-mode(compile).

get_cost([Ns,I|R],Cost) ->
    {N,_} = string:to_integer(Ns),
    NC = {list_to_atom(I),N},
    if R == [] -> [NC|Cost];
       true -> get_cost(tl(R),[NC|Cost])
    end.

parse_blueprint(Line) ->
    Sents = string:lexemes(Line,".:"),
    L = lists:map(fun (Sent) ->
                          Words = string:lexemes(Sent, " "),
                          Res = list_to_atom(lists:nth(2, Words)),
                          Cost = get_cost(lists:nthtail(4, Words),[]),
                          {Res,lists:map(fun (T) ->
                                                 case lists:keyfind(T,1,Cost) of
                                                     {_,V} -> V;
                                                     _ -> 0
                                                 end
                                         end, [ore,clay,obsidian,geode])}
                  end, tl(Sents)),
    maps:from_list(L).

robot_idx(ore) -> 0;
robot_idx(clay) -> 1;
robot_idx(obsidian) -> 2;
robot_idx(geode) -> 3.

can_build(Ores,Cost) ->
    lists:all(fun ({Have,Need}) -> Have >= Need end,
              lists:zip(Ores,Cost)).

take_ores(Ores,Cost) -> lists:zipwith(fun (O,C) -> O-C end, Ores, Cost).

gather(Ores,Robots) -> lists:zipwith(fun (O,R) -> O+R end,Ores,Robots).
add_robot(Robots,Robot) ->
    {H,[C|T]} = lists:split(robot_idx(Robot),Robots),
    H ++ [C+1|T].

estimate(Ores,Robots,TimeLeft) ->
    lists:nth(4,Ores) + lists:nth(4,Robots)*TimeLeft + TimeLeft*(TimeLeft-1)/2.

dfs2(Ores,_,_,_,_,_,_,0) -> lists:nth(4,Ores);
dfs2(Ores,Robots,Costs,GC,MaxUse,NotBuilt,Best,TimeLeft) ->
    NOres = gather(Ores,Robots),
    case can_build(Ores,GC) of
        true ->
            dfs(take_ores(NOres,GC),
                add_robot(Robots,geode),
                Costs,GC,MaxUse,[],Best,TimeLeft-1);
        _ ->
            Rs = lists:filter(fun ({R,C}) ->
                                      HaveR = lists:nth(robot_idx(R)+1,Robots),
                                      (HaveR < lists:nth(robot_idx(R)+1,MaxUse))
                                          and can_build(Ores,C)
                                          and not lists:member(R,NotBuilt)
                              end, maps:to_list(Costs)),
            NInvs = [{take_ores(NOres,C),add_robot(Robots,R),[]} || {R,C} <- Rs],
            NInvs2 = case can_build(Ores,MaxUse) of
                         true -> NInvs;
                         false -> [{NOres,Robots,[X || {X,_} <- Rs]}|NInvs]
                     end,
            lists:foldl(fun ({O,R,NB},B) ->
                                E = estimate(O,R,TimeLeft-1),
                                if E < B ->
                                        ets:insert(cache, {{O,R},{TimeLeft-1,0}}),
                                        B;
                                   true ->
                                        Imp = dfs(O,R,Costs,GC,MaxUse,NB,B,TimeLeft-1),
                                        max(Imp,B)
                                end
                        end, Best, NInvs2)
    end.

dfs(Ores,Robots,Costs,GC,MaxUse,NotBuilt,Best,TimeLeft) ->
    case ets:lookup(cache, {Ores,Robots}) of
        [] ->
            Res = dfs2(Ores,Robots,Costs,GC,MaxUse,NotBuilt,Best,TimeLeft),
            ets:insert(cache, {{Ores,Robots},{TimeLeft,Res}}),
            Res;
        [{_,{T,Value}}] ->
            if T == TimeLeft -> Value;
               T < TimeLeft ->
                    Res = dfs2(Ores,Robots,Costs,GC,MaxUse,NotBuilt,Best,TimeLeft),
                    ets:insert(cache, {{Ores,Robots},{TimeLeft,Res}}),
                    Res;
               T > TimeLeft -> 0
            end
    end.

evaluate(Costs,Turns) ->
    ets:delete_all_objects(cache),
    O = [0,0,0,0],
    R = [1,0,0,0],
    MaxUsage = lists:foldl(fun ({_,C},A) ->
                                   lists:zipwith(fun max/2, C, A)
                           end, lists:duplicate(4,0), maps:to_list(Costs)),
    dfs(O,R,maps:remove(geode,Costs),maps:get(geode,Costs),MaxUsage,[],0,Turns).

main(_) ->
    Blueprints = lists:map(fun parse_blueprint/1, advent:read_all_lines()),
    ets:new(cache, [named_table]),
    io:format("~p~n", [lists:sum(lists:map(fun ({A,B}) -> A*evaluate(B,24) end,
                                           lists:enumerate(Blueprints)))]),
    io:format("~p~n", [lists:foldl(fun (B,A) -> A * evaluate(B,32) end,
                                   1, lists:sublist(Blueprints,3))]).
