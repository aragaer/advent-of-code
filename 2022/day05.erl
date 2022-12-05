#!/usr/bin/env escript

-module(day05).
-export([add_crate/2,add_row/2]).
-mode(compile).

get_count(N, [32,_,32,_|Rest]) -> get_count(N+1, Rest);
get_count(N, _) -> N.

get_crate([91,X,93,_|Rest]) -> {{crate, X}, Rest};
get_crate([32,32,32,_|Rest]) -> {empty, Rest}.

get_crates(Row,[]) -> Row;
get_crates(Row,Rest) ->
    {Item,Left} = get_crate(Rest),
    get_crates([Item|Row],Left).

add_crate(S,{crate,X}) -> [X|S];
add_crate(S,empty) -> S.

add_row(Row,Stacks) ->
    lists:zipwith(fun day05:add_crate/2, Stacks, get_crates([],Row)).

parse_drawing([Indices|Rows]) ->
    Count = get_count(0, Indices),
    Stacks = lists:foldl(fun day05:add_row/2, lists:duplicate(Count,[]), Rows),
    array:from_list(lists:reverse(Stacks)).

get_initial(Acc) ->
    case io:get_line("") of
        "\n" -> parse_drawing(Acc);
        Line -> get_initial([Line|Acc])
    end.

rearrange(Stacks,From,To,Count,Invert) ->
    StackFrom = array:get(From,Stacks),
    StackTo = array:get(To,Stacks),
    {Moved,Left} = lists:split(Count,StackFrom),
    Added = case Invert of
                true -> lists:reverse(Moved);
                _ -> Moved
            end,
    array:set(To,lists:append(Added,StackTo),
              array:set(From,Left,Stacks)).

operate(Stacks1,Stacks2,Line) ->
    {ok, [Count,From,To], _} = io_lib:fread("move ~u from ~u to ~u", Line),
    {rearrange(Stacks1,From-1,To-1,Count,true),
     rearrange(Stacks2,From-1,To-1,Count,false)}.

solve(State1,State2) ->
    case io:get_line("") of
        eof ->
            {lists:map(fun ([X|_]) -> X end, array:to_list(State1)),
             lists:map(fun ([X|_]) -> X end, array:to_list(State2))};
        Line ->
            {NS1,NS2} = operate(State1,State2,Line),
            solve(NS1,NS2)
    end.

main(_) ->
    State = get_initial([]),
    {Res1,Res2} = solve(State,State),
    io:format("~s~n~s~n", [Res1,Res2]).
