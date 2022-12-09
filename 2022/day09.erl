#!/usr/bin/env escript

-mode(compile).

move_once(X,Y,$U) -> {X,Y+1};
move_once(X,Y,$L) -> {X-1,Y};
move_once(X,Y,$R) -> {X+1,Y};
move_once(X,Y,$D) -> {X,Y-1}.

follow_dir(F,T) when F > T -> -1;
follow_dir(F,T) when F < T -> 1;
follow_dir(_,_) -> 0.

follow(HX,HY,TX,TY) ->
    Dist = lists:max([abs(HX-TX),abs(HY-TY)]),
    if Dist < 2 -> {TX,TY};
       true -> {TX+follow_dir(TX,HX),TY+follow_dir(TY,HY)}
    end.

move(Data,Snake,_,0) -> {Data,Snake};
move(Data,[{HX,HY}|Tail],Dir,Dist) ->
    {NX,NY} = move_once(HX,HY,Dir),
    {NewSnake,T} = update_snake(Tail,[{NX,NY}]),
    move(sets:add_element(T, Data), NewSnake,Dir,Dist-1).

update_snake([],Updated) -> {lists:reverse(Updated),hd(Updated)};
update_snake([{TX,TY}|Tail],[{HX,HY}|Updated]) ->
    update_snake(Tail,[follow(HX,HY,TX,TY),{HX,HY}|Updated]).

solve(Data,_,[]) -> sets:size(Data);
solve(Data,Snake,[{Dir,Dist}|Rest]) ->
    {NData,NSnake} = move(Data,Snake,Dir,Dist),
    solve(NData,NSnake,Rest).

get_lines(Acc) ->
    case io:get_line("") of
        eof -> lists:reverse(Acc);
        Line ->
            {ok, [Dir, Dist], _} = io_lib:fread("~c ~u", Line),
            get_lines([{hd(Dir),Dist}|Acc])
    end.

main(_) ->
    Lines = get_lines([]),
    io:format("~p~n",[solve(sets:new(),lists:duplicate(2,{0,0}),Lines)]),
    io:format("~p~n",[solve(sets:new(),lists:duplicate(10,{0,0}),Lines)]).
