#!/usr/bin/env escript

-module(day23).
-mode(compile).
-define(ALLDIRS,[up,upleft,left,downleft,down,downright,right,upright]).

rotate_list([I|Is]) -> Is ++ [I].

propose(Elf,_,[]) -> Elf;
propose(Elf,Taken,[{Dir,Dirs}|Ds]) ->
    case lists:all(fun (D) -> not lists:member(D,Taken) end, Dirs) of
        true -> advent:move(Elf,Dir);
        _ -> propose(Elf,Taken,Ds)
    end.

check(Elf,Elves,DirOrder) ->
    Taken = lists:filtermap(fun (D) -> sets:is_element(advent:move(Elf,D),Elves)
                            end, ?ALLDIRS),
    if Taken == [] -> Elf;
       true -> propose(Elf,Taken,DirOrder)
    end.

area(Elves) ->
    Xs = lists:map(fun ({X,_}) -> X end, Elves),
    Ys = lists:map(fun ({_,Y}) -> Y end, Elves),
    (lists:max(Xs)-lists:min(Xs)+1)*(lists:max(Ys)-lists:min(Ys)+1)-length(Elves).

solve(Elves,DirOrder,N,Round) ->
    SElves = sets:from_list(Elves),
    Proposals = maps:groups_from_list(fun (E) -> check(E, SElves, DirOrder)
                                      end, Elves),
    NoMove = lists:all(fun ({F,T}) -> [F] == T end, maps:to_list(Proposals)),
    if N+1 == Round -> area(Elves);
       NoMove -> Round;
       true -> NElves = lists:flatmap(fun ({Dest,[_]}) -> [Dest];
                                          ({_,Es}) -> Es
                                      end, maps:to_list(Proposals)),
               solve(NElves,rotate_list(DirOrder),N,Round+1)
    end.

main(_) ->
    {Map,_,_} = advent:lines_to_2d_map(advent:read_all_lines()),
    Elves = lists:filtermap(fun ({C,S}) when S == $# -> {true, C};
                                (_) -> false
                            end, maps:to_list(Map)),
    DirOrder = [{down,[downleft,down,downright]},
                {up,[upleft,up,upright]},
                {left,[upleft,left,downleft]},
                {right,[upright,right,downright]}],
    io:format("~p~n~p~n", [solve(Elves,DirOrder,N,1) || N <- [10,-1]]).
