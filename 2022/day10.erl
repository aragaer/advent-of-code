#!/usr/bin/env escript

parse(Lines) ->
    case io:get_line("") of
        eof -> lists:reverse(Lines);
        "noop\n" -> parse([noop|Lines]);
        "addx " ++ Value ->
            {ok, [Num], _} = io_lib:fread("~d", Value),
            parse([{addx, Num}|Lines])
    end.

execute([],_,_,_,Result) -> Result;
execute(Program,Cnt,Tick,X,Result) ->
    {Advance, NX} = case {hd(Program),Cnt} of
                        {noop, 1} -> {true,X};
                        {{addx, Value}, 2} -> {true,X+Value};
                        _ -> {false, X}
                    end,
    NRes = update(Result,Tick,NX),
    if Advance -> execute(tl(Program),1,Tick+1,NX,NRes);
       true -> execute(Program,Cnt+1,Tick+1,X,NRes)
    end.

update({Res1,Res2},Tick,X) ->
    NRes1 = case lists:member(Tick, [20,60,100,140,180,220]) of
                true -> Res1+Tick*X;
                _ -> Res1
            end,
    % do not overwrite top left corner on tick 241
    Pixel = if Tick >= 240 -> array:get((Tick-1) rem 240, Res2);
               abs(X - ((Tick-1) rem 40)) < 2 -> $#;
               true -> $.
            end,
    {NRes1,array:set((Tick-1) rem 240,Pixel,Res2)}.

print_screen([]) -> done;
print_screen(Data) ->
    {Row,Rest} = lists:split(40,Data),
    io:format("~s~n", [Row]),
    print_screen(Rest).

main(_) ->
    {Res1,Res2} = execute(parse([]),0,1,1,{0,array:new([{size,240},{default,$.}])}),
    io:format("~p~n", [Res1]),
    print_screen(array:to_list(Res2)).
