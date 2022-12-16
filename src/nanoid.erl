-module(nanoid).
-export([gen/0]).

-include_lib("kernel/include/logger.hrl").

-import(nanoid_config, [generate/0]).


gen() ->
    erlang:binary_to_list(nanoid_config:generate()).
