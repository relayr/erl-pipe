%%%-------------------------------------------------------------------
%%% @author sjanota
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2015 08:27
%%%-------------------------------------------------------------------
-module(pipe).
-author("sjanota").

-define(HEAD, fun(S, Args) -> [S | Args] end).
-define(TAIL, fun(S, Args) -> Args ++ [S] end).

%% API
-export([
    head/2,
    tail/2
]).

-export([
    stream_from_list/0,
    stream_map/1,
    stream_foreach/1,
    stream_filter/1,
    stream_takewhile/1,
    stream_dropwhile/1,
    stream_to_list/0,
    stream_reduce/2
]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
%% Run pipe appending accumulator as first element to each call
head(Init, Transform) ->
    pipe(Init, Transform, ?HEAD).

%% Run pipe appending accumulator as last element to each call
tail(Init, Transform) ->
    pipe(Init, Transform, ?TAIL).


%%%-------------------------------------------------------------------
%%% Stream API - use with head pipe
%%%-------------------------------------------------------------------
stream_from_list() ->
    fun stream:from_list/1.

stream_map(Fun) ->
    {fun stream:map/2, [Fun]}.

stream_foreach(Fun) ->
    {fun stream:foreach/2, [Fun]}.

stream_filter(Fun) ->
    {fun stream:filter/2, [Fun]}.

stream_takewhile(Fun) ->
    {fun stream:takewhile/2, [Fun]}.

stream_dropwhile(Fun) ->
    {fun stream:dropwhile/2, [Fun]}.

stream_to_list() ->
    fun stream:to_list/1.

stream_reduce(Acc, Fun) ->
    {fun stream:reduce/3, [Acc, Fun]}.
%%%-------------------------------------------------------------------
%%% Local
%%%-------------------------------------------------------------------
pipe(Result, [], _) ->
    Result;
pipe(State0, [H | T], AddState) ->
    State1 = transform(State0, H, AddState),
    pipe(State1, T, AddState).

transform(State, {M, F, A}, AddState) when is_atom(M) and is_atom(F) and is_list(A)->
    erlang:apply(M, F, AddState(State, A));
transform(State, {F, A}, AddState) when is_list(A) and is_function(F, length(A) + 1)->
    erlang:apply(F, AddState(State, A));
transform(State, F, _) when is_function(F, 1)->
    F(State).