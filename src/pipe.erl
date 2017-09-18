-module(pipe).
-author("sjanota").

-include_lib("logger/include/logger.hrl").

-define(HEAD, fun(S, Args) -> S ++ Args end).
-define(TAIL, fun(S, Args) -> Args ++ S end).

%% API
-export([
    head/2,
    tail/2,
    head/3,
    tail/3
]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
%% Run pipe appending accumulator as first element to each call
head(Init, Transform) ->
    pipe(Init, Transform, ?HEAD, []).

%% Run pipe appending accumulator as last element to each call
tail(Init, Transform) ->
    pipe(Init, Transform, ?TAIL, []).

%% Run pipe appending accumulator as first element to each call
head(Init, Transform, Opts) ->
    pipe(Init, Transform, ?HEAD, Opts).

%% Run pipe appending accumulator as last element to each call
tail(Init, Transform, Opts) ->
    pipe(Init, Transform, ?TAIL, Opts).

%%%-------------------------------------------------------------------
%%% Local
%%%-------------------------------------------------------------------
pipe(Result, [], _, Opts) ->
    post_pipe(Result, Opts);
pipe(State0, [H | T], AddState, Opts) ->
    State1 = pre_transform(State0, Opts),
    State2 = transform(State1, H, AddState),
    case post_transform(State2, Opts) of
        {ok, State3} -> pipe(State3, T, AddState, Opts);
        {stop, State3} -> State3
    end.

pre_transform(State, Opts) ->
    case proplists:is_defined(multi_state, Opts) of
        true when is_tuple(State) -> tuple_to_list(State);
        true -> error({bad_state, State});
        false -> [State]
    end.

transform(State, {M, F, A}, AddState) when is_atom(M) and is_atom(F) and is_list(A)->
    erlang:apply(M, F, AddState(State, A));
transform(State, {F, A}, AddState) when is_list(A) and is_function(F, length(A) + length(State))->
    erlang:apply(F, AddState(State, A));
transform(State, F, _) when is_list(State) andalso is_function(F, length(State)) ->
    erlang:apply(F, State).

post_transform(State, Opts) ->
    ChainTerminator = fun(S, _, []) -> {ok, S} end,
    Chain = [
        fun check_flat_ok/3,
        ChainTerminator
    ],
    check_until(State, Opts, Chain).

check_until(State, Opts, [Next | Chain]) ->
    case proplists:get_value(until, Opts) of
        undefined -> Next(State, Opts, Chain);
        Match when is_function(Match, 1) ->
            case Match(State) of
                true -> Next(State, Opts, Chain);
                {false, Return} -> {stop, Return}
            end
    end.

check_flat_ok(State, Opts, [Next | Chain]) ->
    NewState = case proplists:is_defined(flat_ok, Opts) of
        true when is_tuple(State) andalso element(1, State) =:= ok ->
            case tuple_to_list(State) of
                [ok, OkState] -> OkState;
                [ok | OkState] -> list_to_tuple(OkState)
            end;
        true -> error({bad_return, State});
        false ->
            State
    end,
    Next(NewState, Opts, Chain).

post_pipe(Result, Opts) ->
    MultiState = proplists:is_defined(multi_state, Opts),
    case proplists:get_value(flat_ok, Opts) of
        include when is_tuple(Result) andalso MultiState -> list_to_tuple([ok | tuple_to_list(Result)]);
        include -> {ok, Result};
        _ -> Result
    end.
