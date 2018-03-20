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

%% API
-export([]).

-include_lib("testutils/include/testing.hrl").

-ifdef(TEST).

?TEST_FUN().
pipe_lists_operations() ->
    Result = pipe:tail([1,2,3,4], [
        {fun lists:filter/2, [fun(I) -> I rem 2 == 0 end]},
        {fun lists:foldl/3, [fun(I, Acc) -> Acc + I end, 0]}
    ]),
    ?assertEqual(6, Result).

?TEST_FUN().
flat_ok() ->
    Result = pipe:head(2, [
        fun(N) -> {ok, N} end
    ], [flat_ok]),
    ?assertEqual(2, Result),

    BadReturn = fun() -> pipe:head(2, [
        fun(N) -> N end
    ], [flat_ok]) end,
    ?assertError({bad_return, 2}, BadReturn()).

?TEST_FUN().
until() ->
    Increment = fun(N) -> N+1 end,
    GraterThen = fun(Max) -> fun(N) ->
        if N > Max -> {false, {error, N}}; true -> true end
    end end,
    Result = pipe:head(1, [
        Increment, Increment, Increment, Increment
    ], [{until, GraterThen(2)}]),
    ?assertEqual({error, 3}, Result).

?TEST_FUN().
until_with_flat_ok() ->
    Increment = fun(N) -> {ok, N+1} end,
    GraterThen = fun(Max) -> fun({ok, N}) ->
        if N > Max -> {false, {error, N}}; true -> true end
    end end,
    Result = pipe:head(1, [
        Increment, Increment, Increment, Increment
    ], [flat_ok, {until, GraterThen(2)}]),
    ?assertEqual({error, 3}, Result).

?TEST_FUN().
multi_state() ->
    Increment = fun(N1, N2) -> {N1+1, N2+1} end,
    Result = pipe:head({2,3}, [Increment, Increment], [multi_state]),
    ?assertEqual({4,5}, Result).

?TEST_FUN().
multi_state_with_flat_ok() ->
    Increment = fun(N1, N2) -> {ok, N1+1, N2+1} end,
    Result = pipe:head({2,3}, [Increment, Increment], [multi_state, flat_ok]),
    ?assertEqual({4,5}, Result).

-endif.