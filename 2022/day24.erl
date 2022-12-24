#!/usr/bin/env escript

-module(day24).
-mode(compile).

gcd(A,0) -> A;
gcd(A,B) -> gcd(B,A rem B).
lcm(A,B) -> A*B div gcd(A,B).

wrap(C,W,H,Dir) ->
    {NX,NY} = advent:move(C,Dir),
    {(NX-1+W-2) rem (W-2)+1,(NY-1+H-2) rem (H-2)+1}.

get_neighs({C,T},Loop) ->
    [{_,Safe}] = ets:lookup(safe, (T+1) rem Loop),
    NCs = [C|lists:map(advent:move(C), [up,left,down,right])],
    [{NC,T+1} || NC <- lists:filter(fun (NC) -> sets:is_element(NC, Safe) end, NCs)].

stop_at({SX,SY}) ->
    fun ({{X,Y},T}) when X == SX,Y == SY -> T;
        (_) -> false
    end.

main(_) ->
    {Map,Width,Height} = advent:lines_to_2d_map(advent:read_all_lines()),
    Bs = maps:groups_from_list(fun (C) -> maps:get(C,Map) end, maps:keys(Map)),
    Loop = lcm(Width-2,Height-2),
    {Entry,Exit} = {{1,0},{Width-2,Height-1}},
    All = [{X,Y} || {X,Y} <- maps:keys(Map), maps:get({X,Y},Map) /= $#],
    ets:new(safe, [named_table]),
    lists:foldl(fun (T,Blizzards) ->
                        ets:insert(safe,{T,sets:from_list(All--lists:append(Blizzards))}),
                        lists:zipwith(fun (Dir,Lst) ->
                                              [wrap(C,Width,Height,Dir) || C <- Lst]
                                      end, [left,right,down,up], Blizzards)
                end, [maps:get(B,Bs) || B <- "<>^v"], lists:seq(0,Loop-1)),
    {[R1,_,R3],_} = lists:mapfoldl(fun (Dst,{Src,T}) ->
                                           {R,_} = advent:bfs({Src,T},
                                                              Loop,
                                                              fun get_neighs/2,
                                                              stop_at(Dst)),
                                           {R,{Dst,R}}
                                   end, {Entry,0}, [Exit,Entry,Exit]),
    io:format("~p~n~p~n", [R1,R3]).
