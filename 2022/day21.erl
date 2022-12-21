#!/usr/bin/env escript

parse_command(Cmd) ->
    case io_lib:fread("~u", Cmd) of
        {ok, [Num], _} -> {value, Num};
        _ ->
            [Arg1,Op,Arg2] = string:lexemes(Cmd," "),
            {calc, Op, Arg1, Arg2}
    end.

calc("=",{A,human},{_,V}) -> reval(A,V);
calc("=",{_,V},{A,human}) -> reval(A,V);
calc(_,{_,human},_) -> human;
calc(_,_,{_,human}) -> human;
calc("+",{_,V1},{_,V2}) -> V1+V2;
calc("-",{_,V1},{_,V2}) -> V1-V2;
calc("*",{_,V1},{_,V2}) -> V1*V2;
calc("/",{_,V1},{_,V2}) -> V1 div V2.

rcalc("+",{A,human},{_,V1},V) -> reval(A,V-V1);
rcalc("+",{_,V1},{A,human},V) -> reval(A,V-V1);
rcalc("-",{A,human},{_,V1},V) -> reval(A,V+V1);
rcalc("-",{_,V1},{A,human},V) -> reval(A,V1-V);
rcalc("*",{A,human},{_,V1},V) -> reval(A,V div V1);
rcalc("*",{_,V1},{A,human},V) -> reval(A,V div V1);
rcalc("/",{A,human},{_,V1},V) -> reval(A,V*V1);
rcalc("/",{_,V1},{A,human},V) -> reval(A,V1 div V).

eval(Id) ->
    case ets:lookup(cache, Id) of
        [] -> eval(Id,cache_miss);
        [{_,V}] -> V
    end.

eval(Id,_) ->
    {_,V} = hd(ets:lookup(monkeys, Id)),
    case V of
        {value, Val} -> Val;
        human -> human;
        {calc, Op, Arg1, Arg2} ->
            [V1,V2] = [eval(Arg1),eval(Arg2)],
            Res = calc(Op,{Arg1,V1},{Arg2,V2}),
            ets:insert(cache, {Id, Res}),
            Res
    end.

reval(Id, Value) ->
    {_,V} = hd(ets:lookup(monkeys, Id)),
    case V of
        human -> Value;
        {calc, Op, Arg1, Arg2} ->
            rcalc(Op,{Arg1,eval(Arg1)},{Arg2,eval(Arg2)},Value)
    end.

main(_) ->
    Cmds = lists:filtermap(fun ([]) -> false;
                               (L) ->
                                   [Id,Cmd] = string:split(L, ": "),
                                   {true, {Id, parse_command(Cmd)}}
                           end, advent:read_all_lines()),
    ets:new(monkeys, [named_table]),
    ets:new(cache, [named_table]),
    ets:insert(monkeys, Cmds),
    io:format("~p~n", [eval("root")]),
    {_,{calc,_,A1,A2}} = lists:keyfind("root", 1, Cmds),
    NCmds = [{"root", {calc, "=", A1, A2}},
             {"humn", human}],
    ets:insert(monkeys, NCmds),
    ets:delete_all_objects(cache),
    io:format("~p~n", [eval("root")]).
