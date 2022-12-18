-module(advent).
-import(maps,[put/3]).
-import(lists,[filter/2,foldl/3]).
-export([get_int/0,read_all_lines/0,lines_to_2d_map/1,move/2,bfs/3,find_keys/2,move/1]).

get_int() ->
    case io:get_line("") of
        eof -> eof;
        "\n" -> empty;
        Line ->
            {ok, [Num], _} = io_lib:fread("~u", string:chomp(Line)),
            Num
    end.

read_all_lines() -> read_all_lines([]).

read_all_lines(Ls) ->
    case io:get_line("") of
        eof -> lists:reverse(Ls);
        Line -> read_all_lines([string:chomp(Line)|Ls])
    end.

find_keys(Value,Map) -> [K || {K,V} <- maps:to_list(Map), V == Value].

lines_to_2d_map(Lines) ->
    {foldl(fun ({Y,L},D) ->
                   foldl(fun ({X,C},D1) ->
                                 put({X-1,Y-1},C,D1)
                         end, D, lists:enumerate(L))
           end, maps:new(), lists:enumerate(Lines)),
     length(hd(Lines)),length(Lines)}.

move({X,Y},up) -> {X,Y+1};
move({X,Y},down) -> {X,Y-1};
move({X,Y},left) -> {X-1,Y};
move({X,Y},right) -> {X+1,Y};
move({X,Y},upleft) -> {X-1,Y+1};
move({X,Y},upright) -> {X+1,Y+1};
move({X,Y,Z},up) -> {X,Y+1,Z};
move({X,Y,Z},down) -> {X,Y-1,Z};
move({X,Y,Z},left) -> {X-1,Y,Z};
move({X,Y,Z},right) -> {X+1,Y,Z};
move({X,Y,Z},forward) -> {X,Y,Z+1};
move({X,Y,Z},back) -> {X,Y,Z-1}.

% partial
move(C) -> fun (D) -> move(C,D) end.

bfs(Start,Ctx,GetNeighsFun) ->
    bfs([Start],maps:from_list([{Start,0}]),Ctx,GetNeighsFun).

bfs([],Seen,_,_) -> Seen;
bfs([Pos|Queue],Seen,Ctx,GetNeighsFun) ->
    New = filter(fun (N) -> not maps:is_key(N,Seen) end,
                 GetNeighsFun(Pos,Ctx)),
    TimeHere = maps:get(Pos, Seen),
    bfs(lists:append(Queue,New),
        foldl(fun (N,V) -> put(N,TimeHere+1,V) end, Seen, New),
        Ctx, GetNeighsFun).
