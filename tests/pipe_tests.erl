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
-include_lib("logger/include/logger.hrl").

-ifdef(TEST).

?TEST_FUN().
pipe_lists_operations() ->
    Result = pipe:tail([1,2,3,4], [
        {fun lists:filter/2, [fun(I) -> I rem 2 == 0 end]},
        {fun lists:foldl/3, [fun(I, Acc) -> Acc + I end, 0]}
    ]),
    ?assertEqual(6, Result).

?TEST_FUN().
pipe_stream_operations() ->
    Result = pipe:head([1,2,3,4], [
        pipe:stream_from_list(),
        pipe:stream_filter(fun(I) -> I rem 2 == 0 end),
        pipe:stream_map(fun(I) -> I * 2 end),
        pipe:stream_reduce(0, fun(I, Acc) -> Acc + I end)
    ]),
    ?assertEqual(12, Result).

-endif.