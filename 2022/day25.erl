#!/usr/bin/env escript

s2d($=) -> -2;
s2d($-) -> -1;
s2d($0) -> 0;
s2d($1) -> 1;
s2d($2) -> 2.

s2n(S) -> s2n(S,0).
s2n([],N) -> N;
s2n([S|Ss],N) -> s2n(Ss,N*5+s2d(S)).

d2s(-2) -> $=;
d2s(-1) -> $-;
d2s(0) -> $0;
d2s(1) -> $1;
d2s(2) -> $2.

n2s(N) -> n2s(N,[]).
n2s(0,[]) -> "0";
n2s(0,L) -> L;
n2s(N,L) ->
    T = (N+2) rem 5 - 2,
    n2s((N-T) div 5, [d2s(T)|L]).

solve([],A) -> n2s(A);
solve([L|Ls],A) -> solve(Ls,A+s2n(L)).

main(_) ->
    io:format("~s~n", [solve(advent:read_all_lines(),0)]).
