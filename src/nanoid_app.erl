%%%-------------------------------------------------------------------
%% @doc nanoid public API
%% @end
%%%-------------------------------------------------------------------

-module(nanoid_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nanoid_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
