-module(nanoid_config).
-author("Zaryn Technologies").
-export([generate/0, generate/2]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Generates a random string using the specified alphabet and size.
-spec generate() -> bitstring().
generate() ->
    generate(
    <<"_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#$%@*^>][<?}{!">>,
    61).

-spec generate(bitstring(), integer()) -> bitstring().
generate(Alphabet, Size) when Size > 0 ->
    AlpLen = byte_size(Alphabet),
    Mask = calc_mask(AlpLen),
    Step = calc_step(Mask, Size, AlpLen),
    generate_nanoid(Alphabet, Size, Mask, Step, <<"">>);

generate(_, _) ->
    {error, invalid_size}.

%%====================================================================
%% Internal functions
%%====================================================================

generate_nanoid(_Alphabet, Size, _Mask, _Step, Acc) when byte_size(Acc) >= Size ->
    binary:part(Acc, 0, Size);

generate_nanoid(Alphabet, Size, Mask, Step, Acc) ->
    Random = crypto:strong_rand_bytes(Step),
    Nanoid = random_string(Alphabet, Random, Mask, Step, Acc),
    generate_nanoid(Alphabet, Size, Mask, Step, Nanoid).

random_string(_Alphabet, _Random, _Mask, 0, Acc) ->
    Acc;

random_string(Alphabet, Random, Mask, Step, Acc) ->
    Idx = binary:at(Random, Step - 1) band Mask,
    Append = if byte_size(Alphabet) > Idx ->
                    binary:part(Alphabet, Idx, 1);
                true ->
                    <<"">>
             end,
    random_string(Alphabet, Random, Mask, Step - 1, <<Acc/binary, Append/binary>>).

calc_mask(AlpLen) ->
    (2 bsl round(math:floor(math:log(AlpLen - 1)/math:log(2)))) - 1.

calc_step(Mask, Size, AlpLen) ->
    Step = math:ceil(1.6 * Mask * Size / AlpLen),
    round(Step).
