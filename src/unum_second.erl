%%%-------------------------------------------------------------------
%%% @doc unum second implementation, based on record.
%%% @end
%%%-------------------------------------------------------------------
-module(unum_second).
-include_lib("eunit/include/eunit.hrl").
-export([new/0, new/2]).
-export([sign/1, sign/2]).
-export([exponent/1, exponent/2]).
-export([field/1, field/2]).
-export([ubit/1, ubit/2]).
-export([u2f/1]).
-export([to_string/1, to_integer/1, to_bitstring/1, to_float/1, to_meta/1]).
-export([from_bitstring/1, from_float/1]).

-type unum_sign() :: boolean().
-type unum_ubit() :: boolean().
-type unum_exponent() :: pos_integer().
-type unum_field() :: pos_integer().
-type unum_exponentsize() :: pos_integer().
-type unum_fieldsize() :: pos_integer().
-type unum_exponentsizesize() :: pos_integer().
-type unum_fieldsizesize() :: pos_integer().
-record(unum, { sign = false :: unum_sign()
              , exponent = 0:: unum_exponent()
              , field = 0 :: unum_field()
              , ubit = false :: unum_ubit()
              , exponent_size = 1  :: unum_exponentsize()
              , field_size = 1 :: unum_fieldsize()
              }).
-type unum() :: #unum{}.

%%--------------------------------------------------------------------
%% @doc create a new unum object.
%% @end
%%--------------------------------------------------------------------
-spec new() -> Return when
      Return :: unum().
new() ->
    #unum{}.

-spec new(ExponentSize, FieldSize) -> Return when
      ExponentSize :: unum_exponentsize(),
      FieldSize :: unum_fieldsize(),
      Return :: unum().
new(ExponentSize, FieldSize) 
  when ExponentSize > 0 andalso FieldSize > 0 ->
    #unum{ exponent_size = ExponentSize
         , field_size = FieldSize }.

%%--------------------------------------------------------------------
%% @doc get the sign of the unum
%% @end
%%--------------------------------------------------------------------
-spec sign(Unum) -> Return when
      Unum :: unum(),
      Return :: unum_sign().
sign(#unum{ sign = Sign}) ->
    Sign.

-spec sign(Unum, Sign) -> Return when
      Unum :: unum(),
      Sign :: unum_sign(),
      Return :: unum().
sign(#unum{} = Unum, Value)
  when is_boolean(Value) ->
    Unum#unum{ sign = Value }.

sign_test() ->
    ?assertEqual(#unum{ sign = true }, sign(#unum{}, true)),
    ?assertEqual(#unum{ sign = false }, sign(#unum{}, false)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec exponent(Unum) -> Return when
      Unum :: unum(),
      Return :: unum_exponent().
exponent(#unum{ exponent = Exponent }) ->
    Exponent.

-spec exponent(Unum, Exponent) -> Return when
      Unum :: unum(),
      Exponent :: unum_exponent(),
      Return :: unum().
exponent(#unum{} = Unum, Value)
  when is_integer(Value) andalso Value>0 ->
    Unum#unum{ exponent = Value };
exponent(_, _) ->
    erlang:throw(badarg).


exponent_test() ->
    ?assertEqual(#unum{ exponent = 255 }, exponent(#unum{}, 255)),
    ?assertEqual(#unum{ exponent = 1 }, exponent(#unum{}, 1)),
    ?assertThrow(badarg,exponent(#unum{}, 0)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec field(Unum) -> Return when
      Unum :: unum(),
      Return :: unum_field().
field(#unum{ field = Field }) ->
    Field.

-spec field(Unum, Field) -> Return when
      Unum :: unum(),
      Field :: unum_field(),
      Return :: unum().
field(#unum{} = Unum, Value) 
  when is_integer(Value) andalso Value>0 ->
    Unum#unum{ field = Value };
field(_,_) ->
    erlang:throw(badarg).

field_test() ->
    ?assertEqual(#unum{ field = 255 }, field(#unum{}, 255)),
    ?assertEqual(#unum{ field = 1 }, field(#unum{}, 1)),
    ?assertThrow(badarg, field(#unum{}, 0)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec ubit(Unum) -> Return when
      Unum :: unum(),
      Return :: unum_ubit().
ubit(#unum{ ubit = Ubit}) ->
    Ubit.

-spec ubit(Unum, Ubit) -> Return when
      Unum :: unum(),
      Ubit :: unum_ubit(),
      Return :: unum().
ubit(#unum{} = Unum, Value) 
  when is_boolean(Value) ->
    Unum#unum{ ubit = Value }.

ubit_test() ->
    ?assertEqual(#unum{ ubit = true }, ubit(#unum{}, true)),
    ?assertEqual(#unum{ ubit = false }, ubit(#unum{}, false)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
u2f(#unum{ sign = Sign
         , exponent = Exponent
         , exponent_size = ExponentSize
         , field = Field
         , field_size = FieldSize} = _Unum) ->
    u2f(Sign, Exponent, Field, ExponentSize, FieldSize).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
u2f(true, 0, Field, ExponentSize, FieldSize) ->
    calc1(0, Field, ExponentSize, FieldSize);
u2f(false, 0, Field, ExponentSize, FieldSize) ->
    (-1)*calc1(0, Field, ExponentSize, FieldSize);
u2f(true, Exponent, Field, ExponentSize, FieldSize) ->
    calc2(Exponent, Field, ExponentSize, FieldSize);
u2f(false, Exponent, Field, ExponentSize, FieldSize) ->
    (-1)*calc2(Exponent, Field, ExponentSize, FieldSize).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
calc1(Exponent, Field, ExponentSize, FieldSize) ->
    math:pow(2, Exponent-math:pow(2, ExponentSize-1))*(Field/math:pow(2, FieldSize)).

calc2(Exponent, Field, ExponentSize, FieldSize) ->
    math:pow(2, Exponent-math:pow(2, ExponentSize-1)-1)*(1+(Field/math:pow(2, FieldSize))).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
sign_to_integer(false) ->
    0;
sign_to_integer(true) ->
    1.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
ubit_to_integer(false) ->
    0;
ubit_to_integer(true) ->
    1.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
to_string(#unum{} = Unum) ->
    U = u2f(Unum),
    erlang:float_to_list(U).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
to_integer(#unum{} = _Unum) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
to_meta(#unum{} = _Unum) ->
    merl:quote(ok).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
to_bitstring(Unum) ->
    to_bitstring(Unum, 1, 1).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
to_bitstring(#unum{ sign = Sign
                  , exponent = Exponent
                  , field = Field 
                  , ubit = Ubit
                  , exponent_size = ExponentSize
                  , field_size = FieldSize } = _Unum
            , ExponentSizeSize
            , FieldSizeSize ) ->
    S = sign_to_integer(Sign),
    U = ubit_to_integer(Ubit),
    << S:1
     , Exponent:ExponentSize/integer
     , Field:FieldSize/integer
     , U:1
     , ExponentSize:ExponentSizeSize
     , FieldSize:FieldSizeSize 
    >>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
to_float(#unum{} = _Unum) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
from_bitstring(Bitstring) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
from_float(Float) ->
    ok.
