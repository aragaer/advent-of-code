#!/usr/bin/env escript

-module(day22).
-mode(compile).

parse_map(Lines) ->
    lists:foldl(fun ({Y,L},D) ->
                        lists:foldl(fun ({_,$ },D1) -> D1;
                                        ({X,C},D1) -> maps:put({X,Y},C,D1)
                                    end, D, lists:enumerate(L))
                end, maps:new(), lists:enumerate(Lines)).

dir2num(right) -> 0;
dir2num(down) -> 1;
dir2num(left) -> 2;
dir2num(up) -> 3.

move({X,Y},right) -> {X+1,Y};
move({X,Y},left) -> {X-1,Y};
move({X,Y},up) -> {X,Y-1};
move({X,Y},down) -> {X,Y+1}.

rot({X,Y},0) -> {X,Y};
rot({X,Y},$L) -> {Y,1-X};
rot({X,Y},$R) -> {1-Y,X};
rot({X,Y},180) -> {1-X,1-Y}.

bound(V,Mod) -> ((V rem Mod) + Mod - 1) rem Mod + 1.

look(Map,{X,Y},Dir,{Size,Wrap}) ->
    {SX,SY} = {(X-1) div Size, (Y-1) div Size},
    {OBX,OBY} = {SX*Size, SY*Size},
    {OX,OY} = {X-OBX,Y-OBY},
    {NOX,NOY} = move({OX,OY},Dir),
    {NC,T} = if NOX > 0, NOX =< Size, NOY > 0, NOY =< Size -> {{OBX+NOX,OBY+NOY},0};
                true ->
                     {{NSX,NSY},NT} = maps:get({{SX,SY},Dir},Wrap),
                     {AX,AY} = rot({NOX,NOY},NT),
                     {TX,TY} = {bound(AX,Size), bound(AY,Size)},
                     {{NSX*Size+TX,NSY*Size+TY},NT}
             end,
    {NC,maps:get(NC,Map),T}.

try_move(_,C,D,0,_) -> {C,D};
try_move(Map,C,Dir,N,Ctx) ->
    {NC,Tile,Turn} = look(Map,C,Dir,Ctx),
    if Tile == $# -> {C,Dir};
       true -> try_move(Map,NC,turn(Dir,Turn),N-1,Ctx)
    end.

turn(up,0) -> up;
turn(down,0) -> down;
turn(left,0) -> left;
turn(right,0) -> right;
turn(right,$R) -> down;
turn(right,$L) -> up;
turn(left,$R) -> up;
turn(left,$L) -> down;
turn(up,$R) -> right;
turn(up,$L) -> left;
turn(down,$R) -> left;
turn(down,$L) -> right;
turn(right,180) -> left;
turn(left,180) -> right;
turn(up,180) -> down;
turn(down,180) -> up.

turn_where(D1,D2) -> hd(lists:filtermap(fun (R) -> turn(D1,R) == D2 end,
                                        [0,$L,$R,180])).

navigate(Map,Is,C,Dir,Ctx) ->
    {ok, [N], RIs} = io_lib:fread("~u", Is),
    {NC,ND} = try_move(Map,C,Dir,N,Ctx),
    %io:format("Moved from ~p to ~p, looking ~p~n", [C,NC,ND]),
    if RIs == [] ->
            {X,Y} = NC,
            Y * 1000 + X * 4 + dir2num(ND);
       true ->
            NDir = turn(ND,hd(RIs)),
            %io:format("Looking ~p now~n", [NDir]),
            navigate(Map,tl(RIs),NC,NDir,Ctx)
    end.

s2k(S1,S2) -> list_to_tuple(lists:sort([S1,S2])).
find_neighbours({X,Y},Sides) ->
    Ns = lists:filtermap(fun (D) ->
                                 NC = move({X,Y},D),
                                 case lists:member(NC,Sides) of
                                     true -> {true, {D, NC}};
                                     _ -> false
                                 end
                         end, [up,left,down,right]),
    %io:format("~p: ~p~n", [Side, Ns]),
    lists:map(fun ({D, N}) ->
                      {F,T} = s2k({X,Y},N),
                      ND = if F == {X,Y} -> D;
                              true -> turn(D, 180)
                           end,
                      {{F,T},{ND,0}}
              end, Ns).

known_neighs(S,Edges,Sides) ->
    lists:filter(fun (S1) when S /= S1 ->
                         Rel = maps:get(s2k(S,S1),Edges),
                         not lists:member(Rel, [unknown,opposite]);
                     (_) -> false
                 end, Sides).

rev($L) -> $R;
rev($R) -> $L;
rev(0) -> 0;
rev(180) -> 180.

back($L) -> $R;
back($R) -> $L;
back(0) -> 180;
back(180) -> 0.

combine(T1,T2) -> turn_where(up,turn(turn(up,T1),T2)).

edge_dir(S1,S2,Edges) ->
    K = s2k(S1,S2),
    {D,T} = maps:get(K,Edges),
    if K == {S1,S2} -> {D,T};
       true -> {turn(D,back(T)),rev(T)}
    end.

glue_sides(Edges,Sides) ->
    Unknowns = [K || {K,V} <- maps:to_list(Edges), V == unknown],
    CanGlue = lists:filtermap(fun ({S1,S2}) ->
                                      N1 = known_neighs(S1,Edges,Sides),
                                      N2 = known_neighs(S2,Edges,Sides),
                                      Common = sets:intersection(sets:from_list(N1),
                                                                 sets:from_list(N2)),
                                      case sets:is_empty(Common) of
                                          true -> false;
                                          _ -> {true, {S1,S2,hd(sets:to_list(Common))}}
                                      end
                              end, Unknowns),
    NEs = lists:foldl(fun ({S1,S2,S},Es) ->
                              {D1,T1} = edge_dir(S,S1,Es),
                              {D2,T2} = edge_dir(S,S2,Es),
                              D = turn(D1,combine(T1,turn_where(D1,D2))),
                              T = combine(T2,combine(rev(T1),turn_where(D1,D2))),
                              case turn_where(D1,D2) of
                                  180 -> maps:put({S1,S2},opposite,Es);
                                  _ -> maps:put({S1,S2},{D,T},Es)
                              end
                      end, Edges, CanGlue),
    case length([1 || {_,V} <- maps:to_list(NEs), V == unknown]) of
        0 -> NEs;
        _ -> glue_sides(NEs,Sides)
    end.

make_cube(Sides,Edges) ->
    Init = lists:foldl(fun (Side,Es) ->
                               maps:merge(Es,maps:from_list(find_neighbours(Side, Sides)))
                       end, Edges, Sides),
    glue_sides(Init,Sides).

go_back(C,Dir,Sides) ->
    NC = move(C,Dir),
    case lists:member(NC, Sides) of
        true -> go_back(NC, Dir, Sides);
        _ -> C
    end.

wrap(C,Dir,Sides) ->
    NC = move(C,Dir),
    case lists:member(NC, Sides) of
        true -> NC;
        _ -> go_back(C,turn(Dir,180),Sides)
    end.

main(_) ->
    Lines = advent:read_all_lines(),
    Instructions = lists:last(Lines),
    Map = parse_map(lists:takewhile(fun ([]) -> false;
                                        (_) -> true
                                    end, Lines)),
    Size = trunc(math:sqrt(maps:size(Map) div 6)),
    Sides = [{X,Y} || Y <- lists:seq(0,3),
                      X <- lists:seq(0,3),
                      maps:is_key({X*Size+1,Y*Size+1},Map)],
    Start = list_to_tuple([X * Size+1 || X <- tuple_to_list(hd(Sides))]),
    Edges = maps:from_keys([{F,T} || F <- Sides, T <- Sides, F < T], unknown),
    NEdges = make_cube(Sides,Edges),
    Wrapped = maps:from_list([{{S,D},{wrap(S,D,Sides),0}}
                              || S <- Sides, D <- [up,left,down,right]]),
    Glued = maps:from_list(lists:flatmap(fun ({_,opposite}) -> [];
                                             ({{S1,S2},{D,T}}) ->
                                                 [{{S1,D},{S2,T}},
                                                  {{S2,turn(D,back(T))},{S1,rev(T)}}]
                                         end, maps:to_list(NEdges))),
    io:format("~p~n~p~n", [navigate(Map,Instructions,Start,right,{Size,Wrapped}),
                           navigate(Map,Instructions,Start,right,{Size,Glued})]).
