%%%-------------------------------------------------------------------
%%% @author sjanota
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2015 09:59
%%%-------------------------------------------------------------------
-module(pipe_tests).
-author("sjanota").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pipe_lists_operations_test() ->
    Result = pipe:tail([1,2,3,4], [
        {fun lists:filter/2, [fun(I) -> I rem 2 == 0 end]},
        {fun lists:foldl/3, [fun(I, Acc) -> Acc + I end, 0]}
    ]),
    ?assertEqual(6, Result).

flat_ok_test() ->
    Result = pipe:head(2, [
        fun(N) -> {ok, N} end
    ], [flat_ok]),
    ?assertEqual(2, Result),

    BadReturn = fun() -> pipe:head(2, [
        fun(N) -> N end
    ], [flat_ok]) end,
    ?assertError({bad_return, 2}, BadReturn()).

until_test() ->
    Increment = fun(N) -> N+1 end,
    GraterThen = fun(Max) -> fun(N) ->
        if N > Max -> {false, {error, N}}; true -> true end
    end end,
    Result = pipe:head(1, [
        Increment, Increment, Increment, Increment
    ], [{until, GraterThen(2)}]),
    ?assertEqual({error, 3}, Result).

until_with_flat_ok_test() ->
    Increment = fun(N) -> {ok, N+1} end,
    GraterThen = fun(Max) -> fun({ok, N}) ->
        if N > Max -> {false, {error, N}}; true -> true end
    end end,
    Result = pipe:head(1, [
        Increment, Increment, Increment, Increment
    ], [flat_ok, {until, GraterThen(2)}]),
    ?assertEqual({error, 3}, Result).

multi_state_test() ->
    Increment = fun(N1, N2) -> {N1+1, N2+1} end,
    Result = pipe:head({2,3}, [Increment, Increment], [multi_state]),
    ?assertEqual({4,5}, Result).

multi_state_with_flat_ok_test() ->
    Increment = fun(N1, N2) -> {ok, N1+1, N2+1} end,
    Result = pipe:head({2,3}, [Increment, Increment], [multi_state, flat_ok]),
    ?assertEqual({4,5}, Result).

-endif.
