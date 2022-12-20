#!/usr/bin/env escript

-module(day20).
-mode(compile).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> X rem Y + Y;
mod(0,_) -> 0.

mix(Data,Threaded) ->
    L = length(Data),
    Align = fun (X) -> mod(X + L div 2, L-1) - L div 2 end,
    lists:foldl(fun ({I,X},T) ->
                        threaded_move(T,I,Align(X))
%                        case threaded2list(Res) of
%                            broken ->
%                                io:format("Broken after moving ~p from ~p by ~p~n",
%                                          [X,I,Align(X)]),
%                                error(broken);
%                            _ -> Res
%                        end
                end,
                Threaded, lists:enumerate(Data)).

threaded_cut({D,Is,Rs},I) ->
    Next = array:get(I,Is),
    Prev = array:get(I,Rs),
    NIs = array:set(Prev,Next,Is),
    NRs = array:set(Next,Prev,Rs),
    First = array:get(0,Is),
    Last = array:get(0,Rs),
    {D,
     if I == First -> array:set(0,Next,NIs);
        true -> NIs
     end,
     if I == Last -> array:set(0,Prev,NRs);
        true  -> NRs
     end}.

threaded_insert({D,Is,Rs},I,NI) ->
    Next = array:get(NI,Is),
    Prev = NI,
    NRs = array:set(Next,I,array:set(I,Prev,Rs)),
    Last = array:get(0,Rs),
    {D,
     array:set(Prev,I,array:set(I,Next,Is)),
     if Last == Prev -> array:set(0,I,NRs);
        true -> NRs
     end}.


threaded_shift(_,I,0) -> I;
threaded_shift({_,_,Rs},I,-1) -> array:get(array:get(I,Rs),Rs);
threaded_shift({D,Is,Rs},I,N) when N > 0 ->
    threaded_shift({D,Is,Rs},array:get(I,Is),N-1);
threaded_shift({D,Is,Rs},I,N) ->
    threaded_shift({D,Is,Rs},array:get(I,Rs),N+1).

threaded_move(T,_,0) -> T;
threaded_move(T,I,By) ->
    %io:format("Move digit ~p~n", [array:get(I-1,Data)]),
    Cut = threaded_cut(T,I),
    NI = threaded_shift(T,I,By),
    %io:format("move from ~p by ~p to ~p~n", [I,By,array:get(NI,Is)]),
    %{_,CIs,_} = Cut,
    %io:format("New position: ~p (between ~p and ~p) ~n",
    %          [NI, array:get(NI-1,Data), array:get(array:get(NI,CIs)-1,Data)]),
    %io:format("After cut: ~p~n", [threaded2list(Cut)]),
    threaded_insert(Cut,I,NI).

list2threaded(List) ->
    Arr = array:from_list(lists:seq(1,length(List))),
    RevArr = array:from_list([length(List),length(List)|lists:seq(1,length(List)-1)]),
    {array:fix(array:from_list(List)),
     array:fix(array:set(length(List),1,Arr)),
     array:fix(RevArr)}.
%threaded2list({Data,_,Rs}) ->
%    Head = array:get(0,Rs),
%    threaded_collect(Data,Rs,Head,Head,[],0,array:size(Data)+1).
%
%threaded_collect(_,Rs,_,Stop,_,D,L) when D > L ->
%    io:format("Loop incorrect: can't reach ~p~n", [Stop]),
%    broken;
%threaded_collect(Data,Rs,Ptr,Stop,Acc,Depth,Lim) ->
%    Item = array:get(Ptr-1,Data),
%    Prev = array:get(Ptr,Rs),
%    if Prev == Stop -> [Item|Acc];
%       true -> threaded_collect(Data,Rs,Prev,Stop,[Item|Acc],Depth+1,Lim)
%    end.

threaded_index_of({Data,Is,_},Needle) ->
    threaded_index_of(Data,Is,Needle,array:get(0,Is)).
threaded_index_of(Data,Is,Needle,Ptr) ->
    Value = array:get(Ptr-1,Data),
    if Value == Needle -> Ptr;
       true -> threaded_index_of(Data,Is,Needle,array:get(Ptr,Is))
    end.

result(Mixed) ->
    {MD,_,_} = Mixed,
    {_,R} = lists:foldl(fun (Offt,{B,S}) ->
                                P = threaded_shift(Mixed,B,Offt),
                                {P,S+array:get(P-1,MD)}
                        end, {threaded_index_of(Mixed,0),0}, [1000,1000,1000]),
    R.

main(_) ->
    Data = lists:map(fun (L) ->
                             {ok, [Num], _} = io_lib:fread("~d", L),
                             Num
                     end, advent:read_all_lines()),
    Threaded = list2threaded(Data),
    Mixed = mix(Data,Threaded),
    Data2 = [I * 811589153 || I <- Data],
    Threaded2 = list2threaded(Data2),
    Mixed2 = lists:foldl(fun (_,M) -> mix(Data2,M) end, Threaded2, lists:seq(1,10)),
    io:format("~p~n~p~n",[result(Mixed),result(Mixed2)]).
