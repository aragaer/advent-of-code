#!/usr/bin/env escript

-module(day18).
-import(advent,[move/1,move/2]).
-mode(compile).

cube_to_sides(C) ->
    [{left,C},
     {top,C},
     {front,C},
     {left,move(C,right)},
     {top,move(C,down)},
     {front,move(C,back)}].

disunion(S,L) ->
    lists:foldr(fun (E,S1) ->
                        case sets:is_element(E,S1) of
                            true -> sets:del_element(E,S1);
                            false -> sets:add_element(E,S1)
                        end
                end, S, L).

get_neighs(C,{Cubes,Left,Right,Bottom,Top,Back,Front}) ->
    lists:filter(fun ({X,_,_}) when X < Left -> false;
                     ({X,_,_}) when X > Right -> false;
                     ({_,Y,_}) when Y < Bottom -> false;
                     ({_,Y,_}) when Y > Top -> false;
                     ({_,_,Z}) when Z < Back -> false;
                     ({_,_,Z}) when Z > Front -> false;
                     (C1) -> not lists:member(C1,Cubes)
                 end, lists:map(move(C), [up,down,left,right,forward,back])).

main(_) ->
    CubesL = lists:map(fun (L) ->
                               lists:map(fun (D) ->
                                                 {X,_} = string:to_integer(D),
                                                 X
                                         end, string:lexemes(L, ","))
                       end, advent:read_all_lines()),
    Cubes = lists:map(fun list_to_tuple/1, CubesL),
    Xs = lists:map(fun ({X,_,_}) -> X end, Cubes),
    Ys = lists:map(fun ({_,Y,_}) -> Y end, Cubes),
    Zs = lists:map(fun ({_,_,Z}) -> Z end, Cubes),
    [Left,Right,Bottom,Top,Back,Front] = [F(L) || L <- [Xs,Ys,Zs],
                                                  F <- [fun lists:min/1,
                                                        fun lists:max/1]],
    Ctx = {Cubes,Left,Right,Bottom,Top,Back,Front},
    Start = {Top,Right,Front},
    Outer = maps:keys(advent:bfs(Start,Ctx,fun get_neighs/2)),
    Sides = lists:foldr(fun (C,S) -> disunion(S,cube_to_sides(C)) end,
                        sets:new(), Cubes),
    Inner = ([{X,Y,Z} || X <- lists:seq(Left,Right),
                         Y <- lists:seq(Bottom,Top),
                         Z <- lists:seq(Back,Front)] -- Outer) -- Cubes,
    Sides2 = lists:foldr(fun (C,S) -> disunion(S,cube_to_sides(C)) end,
                         Sides, Inner),
    io:format("~p~n~p~n",[sets:size(Sides),
                          sets:size(Sides2)]).
