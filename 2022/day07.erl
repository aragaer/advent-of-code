#!/usr/bin/env escript

-module(day07).
-mode(compile).

path_to_key(Name,Path) ->
    lists:concat(lists:join("/", lists:reverse([Name|Path]))).

parse(Collected,Path) ->
    case io:get_line("") of
        eof -> Collected;
        "$ cd /\n" -> parse(Collected,[""]);
        "$ cd ..\n" -> parse(Collected,tl(Path));
        "$ cd " ++ Dir -> parse(Collected,[string:chomp(Dir)|Path]);
        "$ ls\n" -> parse(Collected,Path); % do nothing actually
        "dir " ++ Dir ->
            Name = string:chomp(Dir),
            parse(maps:put(path_to_key(Name,Path),{dir,undefined},Collected),Path);
        Line -> % we've got a file
            {ok, [Size,Name], _} = io_lib:fread("~u ~s\n", Line),
            parse(maps:put(path_to_key(Name,Path),{file,Size},Collected),Path)
    end.

is_child(Parent,Path) ->
    case string:prefix(Path,Parent) of
        nomatch -> false;
        Rest -> not lists:member($/,Rest)
    end.

get_size(Tree,Path) ->
    case maps:get(Path,Tree) of
        {file,Size} -> {Tree,Size};
        {dir,undefined} ->
            Key = string:concat(Path,"/"),
            Contents = maps:filter(fun (K,_) -> is_child(Key,K) end, Tree),
            {Sized,Total} = lists:foldl(fun (P,{T,S}) ->
                                                {T1,S1} = get_size(T,P),
                                                {T1,S1+S}
                                        end, {Tree,0}, maps:keys(Contents)),
            {maps:update(Path,{dir,Total},Sized),Total};
        {dir,Size} -> {Tree,Size}
    end.

part1(Tree) ->
    maps:fold(fun (_,{dir,Size},Acc) when Size =< 100000 -> Acc+Size;
                  (_,_,Acc) -> Acc
              end, 0, Tree).

part2(Tree,TotalRoot) ->
    Needed = TotalRoot - 40000000,
    maps:fold(fun (_,{dir,Size},Acc) when Size>=Needed,Size<Acc -> Size;
                  (_,_,Acc) -> Acc
              end, TotalRoot, Tree).

main(_) ->
    Tree = parse(maps:put("",{dir,undefined},maps:new()),[""]),
    {Sized,TotalRoot} = get_size(Tree,""),
    io:format("~p~n~p~n", [part1(Sized),part2(Sized,TotalRoot)]).
