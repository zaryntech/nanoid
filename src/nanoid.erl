-module(nanoid).
-author("Zaryn Technologies").
-export([gen/0]).
-include_lib("kernel/include/logger.hrl").
-import(nanoid_config, [generate/0]).

%% Main function to generate random key 
gen() ->
    erlang:binary_to_list(nanoid_config:generate()).
