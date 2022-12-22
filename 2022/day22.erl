#!/usr/bin/env escript

-module(day22).
-mode(compile).
-define(UP, {0,-1}).
-define(DOWN, {0,1}).
-define(LEFT, {-1,0}).
-define(RIGHT, {1,0}).
-define(T_0, {1,0,0,1}).
-define(T_CW, {0,-1,1,0}).
-define(T_CCW, {0,1,-1,0}).
-define(T_180, {-1,0,0,-1}).

parse_map(Lines) ->
    lists:foldl(fun ({Y,L},D) ->
                        lists:foldl(fun ({_,$ },D1) -> D1;
                                        ({X,C},D1) -> maps:put({X,Y},C,D1)
                                    end, D, lists:enumerate(L))
                end, maps:new(), lists:enumerate(Lines)).

dir2num(?RIGHT) -> 0;
dir2num(?DOWN) -> 1;
dir2num(?LEFT) -> 2;
dir2num(?UP) -> 3.

move({X,Y},{DX,DY}) -> {X+DX,Y+DY}.
turn({X,Y},{Axx,Axy,Ayx,Ayy}) -> {X*Axx+Y*Axy,X*Ayx+Y*Ayy}.
rev({Axx,Axy,Ayx,Ayy}) -> {Axx,Ayx,Axy,Ayy}.
back(D) -> list_to_tuple([-X || X <- tuple_to_list(D)]).
turn_where({A,B},{C,D}) -> {A*C+B*D,C*B-A*D,A*D-C*B,A*C+B*D}.
combine({A,B,C,D},{E,F,G,H}) -> {A*E+B*G,A*F+B*H,C*E+D*G,C*F+D*H}.

bound(V,Mod) -> ((round(V) rem Mod) + Mod - 1) rem Mod + 1.

look(Map,{X,Y},Dir,{Size,Wrap}) ->
    {SX,SY} = {(X-1) div Size, (Y-1) div Size},
    {OBX,OBY} = {SX*Size, SY*Size},
    {OX,OY} = {X-OBX,Y-OBY},
    {NOX,NOY} = move({OX,OY},Dir),
    {NC,T} = if NOX > 0, NOX =< Size, NOY > 0, NOY =< Size -> {{OBX+NOX,OBY+NOY},?T_0};
                true ->
                     {{NSX,NSY},NT} = maps:get({{SX,SY},Dir},Wrap),
                     {AX,AY} = turn({NOX-0.5,NOY-0.5},NT),
                     {TX,TY} = {bound(AX+0.5,Size), bound(AY+0.5,Size)},
                     {{NSX*Size+TX,NSY*Size+TY},NT}
             end,
    {NC,maps:get(NC,Map),T}.

try_move(_,C,D,0,_) -> {C,D};
try_move(Map,C,Dir,N,Ctx) ->
    {NC,Tile,Turn} = look(Map,C,Dir,Ctx),
    if Tile == $# -> {C,Dir};
       true -> try_move(Map,NC,turn(Dir,Turn),N-1,Ctx)
    end.

navigate(Map,Is,C,Dir,Ctx) ->
    {ok, [N], RIs} = io_lib:fread("~u", Is),
    {NC,ND} = try_move(Map,C,Dir,N,Ctx),
    %io:format("Moved from ~p to ~p, looking ~p~n", [C,NC,ND]),
    if RIs == [] ->
            {X,Y} = NC,
            Y * 1000 + X * 4 + dir2num(ND);
       true ->
            T = case hd(RIs) of
                    $L -> ?T_CCW;
                    $R -> ?T_CW
                end,
            NDir = turn(ND,T),
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
                         end, [?UP,?LEFT,?DOWN,?RIGHT]),
    %io:format("~p: ~p~n", [Side, Ns]),
    lists:map(fun ({D, N}) ->
                      {F,T} = s2k({X,Y},N),
                      ND = if F == {X,Y} -> D;
                              true -> turn(D, ?T_180)
                           end,
                      {{F,T},{ND,?T_0}}
              end, Ns).

known_neighs(S,Edges,Sides) ->
    lists:filter(fun (S1) when S /= S1 ->
                         Rel = maps:get(s2k(S,S1),Edges),
                         not lists:member(Rel, [unknown,opposite]);
                     (_) -> false
                 end, Sides).

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
                                  ?T_180 -> maps:put({S1,S2},opposite,Es);
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
        _ -> go_back(C,turn(Dir,?T_180),Sides)
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
    Wrapped = maps:from_list([{{S,D},{wrap(S,D,Sides),?T_0}}
                              || S <- Sides, D <- [?UP,?LEFT,?DOWN,?RIGHT]]),
    Glued = maps:from_list(lists:flatmap(fun ({_,opposite}) -> [];
                                             ({{S1,S2},{D,T}}) ->
                                                 [{{S1,D},{S2,T}},
                                                  {{S2,turn(D,back(T))},{S1,rev(T)}}]
                                         end, maps:to_list(NEdges))),
    io:format("~p~n~p~n", [navigate(Map,Instructions,Start,?RIGHT,{Size,Wrapped}),
                           navigate(Map,Instructions,Start,?RIGHT,{Size,Glued})]).
