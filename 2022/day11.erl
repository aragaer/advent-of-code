#!/usr/bin/env escript

-mode(compile).

op_or_val(old,Old) -> Old;
op_or_val(Val,_) -> Val.

parse_operation(Expression) ->
    [Arg1,Op,Arg2] = lists:map(fun ("old") -> old;
                                   ("+") -> fun (A,B) -> A+B end;
                                   ("*") -> fun (A,B) -> A*B end;
                                   (X) -> {Int, _} = string:to_integer(X),
                                          Int
                               end, string:tokens(Expression, " ")),
    fun (X) -> Op(op_or_val(Arg1,X),op_or_val(Arg2,X)) end.

parse_action(TestLine,TrueLine,FalseLine) ->
    {ok, [DivBy], _} = io_lib:fread("  Test: divisible by ~u", TestLine),
    TrueTo = lists:last(string:tokens(TrueLine, " ")),
    FalseTo = lists:last(string:tokens(FalseLine, " ")),
    {fun (Items,Item) ->
             ThrowTo = if Item rem DivBy == 0 -> TrueTo;
                          true -> FalseTo
                       end,
             maps:update_with(ThrowTo, fun (L) -> [Item|L] end, Items)
     end, DivBy}.

parse_monkey(Lines) ->
    [L0,L1,L2,L3,L4,L5] = lists:map(fun string:chomp/1, Lines),
    Id = lists:droplast(string:prefix(L0, "Monkey ")),
    Items = lists:map(fun (X) -> {Int, _} = string:to_integer(X),
                                 Int end,
                      string:tokens(string:prefix(L1, "  Starting items: "),", ")),
    {monkey,Id,lists:reverse(Items),
     parse_operation(string:prefix(L2, "  Operation: new = ")),
     parse_action(L3,L4,L5)}.

parse(Monkeys,Items,Rem) ->
    L0 = io:get_line(""),
    if L0 == eof -> {lists:reverse(Monkeys),Items,Rem};
       L0 == "\n" -> parse(Monkeys,Items,Rem);
       true ->
            Lines = [L0|lists:map(fun (_) -> io:get_line("") end, [1,2,3,4,5])],
            {monkey,Id,ItemsHere,F1,{F2,D}} = parse_monkey(Lines),
            parse([{Id,F1,F2}|Monkeys],maps:put(Id,ItemsHere,Items),Rem*D)
    end.

monkey_turn(_, _, [], Items, _) -> Items;
monkey_turn(F1, F2, [Item|Rest], Items, 0) ->
    UpdatedItem = F1(Item) div 3,
    monkey_turn(F1, F2, Rest, F2(Items, UpdatedItem), 0);
monkey_turn(F1, F2, [Item|Rest], Items, Rem) ->
    UpdatedItem = F1(Item) rem Rem,
    monkey_turn(F1, F2, Rest, F2(Items, UpdatedItem), Rem).

turn([],Items,Throws,_) -> {Items,Throws};
turn([{Id,F1,F2}|Monkeys],Items,Throws,Rem) ->
    ItemsHere = lists:reverse(maps:get(Id,Items)),
    NewItems = monkey_turn(F1, F2, ItemsHere, maps:put(Id,[],Items),Rem),
    turn(Monkeys, NewItems,
         maps:update_with(Id, fun (T) -> T+length(ItemsHere) end, Throws),
         Rem).

many_turns(_,Items,Throws,_,0) -> {Items,Throws};
many_turns(Monkeys,Items,Throws,Rem,N) ->
    {NItems,NThrows} = turn(Monkeys,Items,Throws,Rem),
    many_turns(Monkeys,NItems,NThrows,Rem,N-1).

solve(Monkeys,Items,Rem,Turns) ->
    T0 = lists:foldl(fun ({Id,_,_},T) -> maps:put(Id,0,T) end, maps:new(), Monkeys),
    {_,NThrows} = many_turns(Monkeys,Items,T0,Rem,Turns),
    [T1,T2|_] = lists:reverse(lists:sort(maps:values(NThrows))),
    io:format("~p~n", [T1*T2]).

main(_) ->
    {Monkeys,Items,Rem} = parse([],maps:new(),1),
    solve(Monkeys,Items,0,20),
    solve(Monkeys,Items,Rem,10000).
