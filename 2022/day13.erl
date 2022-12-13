#!/usr/bin/env escript

-module(day13).
-import(lists,[append/2,enumerate/1,filtermap/2,foldl/3,sort/2]).
-mode(compile).

cmp([],_) -> true;
cmp(_,[]) -> false;
cmp([A|As],[B|Bs]) ->
    if A == B -> cmp(As,Bs);
       is_list(A),is_list(B) -> cmp(A,B);
       is_list(A) -> cmp([A|As],[[B]|Bs]);
       is_list(B) -> cmp([[A]|As],[B|Bs]);
       true -> A =< B
    end.

main(_) ->
    Lines = filtermap(fun ([]) -> false;
                          (L) ->
                              {ok,Ts,_} = erl_scan:string(append(L,".")),
                              {ok,R} = erl_parse:parse_term(Ts),
                              {true,R}
                      end,
                      advent:read_all_lines()),
    io:format("~p~n", [foldl(fun ({I,L2},{L1,R}) ->
                                     R + case cmp(L1,L2) of
                                             true -> I div 2;
                                             _ -> 0
                                         end;
                                 ({_,L1},R) -> {L1,R}
                             end, 0, enumerate(Lines))]),
    Part2 = foldl(fun ({I,[[2]]},A) -> I*A;
                      ({I,[[6]]},A) -> I*A;
                      (_,A) -> A
                  end, 1, enumerate(sort(fun cmp/2, [[[2]],[[6]]|Lines]))),
    io:format("~p~n", [Part2]).
