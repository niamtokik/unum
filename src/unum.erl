-module(unum).
-export([encode/1, encode/2, encode/3]).
-export([decode/1, decode/2, decode/3]).
-include_lib("eunit/include/eunit.hrl").

-type exponent_size() :: integer().
-type fraction_size() :: integer().
-type sizes() :: {exponent_size(), fraction_size()}.
-type unum() :: binary().

%%--------------------------------------------------------------------
%% @doc encoding functions
%%--------------------------------------------------------------------
-spec encode(Binary) -> Unum when
      Binary :: bitstring(),
      Unum :: unum().
encode(Binary) ->
    encode(Binary, {2,3}).

-spec encode(Binary, Sizes) -> Unum when
      Binary :: bitstring(),
      Sizes :: sizes(),
      Unum :: unum().
encode(Binary, Sizes) ->
    encode(Binary, Sizes, []).

-spec encode(Binary, Sizes, Options) -> Unum when
      Binary :: bitstring(),
      Sizes :: sizes(),
      Options :: list(),
      Unum :: unum().
encode(_Binary, {_ExponentSizeSize, _FractionSizeSize}, _Options) ->
    ok.

%%--------------------------------------------------------------------
%% @doc encoding functions unit test
%%--------------------------------------------------------------------
encode_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc decoding functions
%%--------------------------------------------------------------------
-spec decode(Binary) -> Unum when
      Binary :: bitstring(),
      Unum :: unum().
decode(Binary) ->
    decode(Binary, {2,3}).

-spec decode(Binary, Sizes) -> Unum when
      Binary :: bitstring(),
      Sizes :: sizes(),
      Unum :: unum().
decode(Binary, Sizes) ->
    decode(Binary, Sizes, []).

-spec decode(Binary, Sizes, Options) -> Unum when
      Binary :: bitstring(),
      Sizes :: sizes(),
      Options :: list(),
      Unum :: unum().
decode(Binary, {ExponentSizeSize, FractionSizeSize}, _Options) ->
    <<FractionSize:FractionSizeSize, ExponentSize:ExponentSizeSize, Rest/bitstring>> = Binary,
    FS = FractionSize+1,
    ES = ExponentSize+1,
    case Rest of
        <<Ubit:1, F:FS, E:ES, S:1>> -> {Ubit, F, E, S};
        <<Ubit:1, F:FS, E:ES, S:1, R/bitstring>> -> {Ubit, F, E, S, R}
    end.

%%--------------------------------------------------------------------
%% @doc decoding functions unit test
%%--------------------------------------------------------------------
decode_test() ->
    ?assertEqual(decode(<<2#111:3, 2#11:2, 2#1:1, 2#00000000:8, 2#0000:4, 2#0:1>>, {2,3})
                ,{0, 0.00006103515625}),
    ?assertEqual(decode(<<2#111:3, 2#11:2, 2#1:1, 2#00000000:8, 2#0000:4, 2#1:1>>, {2,3})
                ,{-0.00006103515625, 0}),
    ?assertEqual(decode(<<2#0:3, 2#0:2, 2#1:1, 2#0:1, 2#1:1>>, {2,3})
                ,{-1, 0}),
    ?assertEqual(decode(<<2#1111:4, 2#0:1, 2#1:1, 2#1001001000011111:16, 2#1:1, 2#0:1>>)
                ,{3.141571044921875, 3.1416015625}).
