-module(unum_try).
-compile(export_all).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-record(state, { field_size = 1 :: pos_integer()
               , exponent_size = 1  :: pos_integer()
               , endian = big :: atom()
               , sign = 0 :: integer()
               , exponent = 0 :: pos_integer()
               , fraction = 0 :: pos_integer()
               , ubit = 0 :: integer()
               }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Args) ->
    FieldSize = proplists:get_value(field_size, Args, 1),
    ExponentFieldSize = proplists:get_value(exponent_size, Args, 1),
    {ok, #state{ field_size = FieldSize
               , exponent_size = ExponentFieldSize }}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(_,_) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast({set, field_size, Value}, State) 
  when is_integer(Value) ->
    {noreply, State#state{ field_size = Value }};
handle_cast({set, exponent_size, Value}, State) 
  when is_integer(Value) ->
    {noreply, State#state{ exponent_size = Value }};
handle_cast({set, endian, Value}, State) 
  when is_atom(Value) ->
    {noreply, State#state{ endian = Value }}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call({bitstring, Bitstring}, _, #state{ field_size = FS
                                             , exponent_size = ES 
                                             } = State) ->
    
    try 
        <<Sign:1, Exponent:ES, Field:FS, Ubit:1>> = Bitstring,
        View = {Sign, Exponent, Field, Ubit},
        U2F = fun ({Sign, 0 = Exponent, Field, Ubit}) ->
                      math:pow(-1, Sign)*math:pow(2, Exponent-math:pow(2, ES-1))*(Field/math:pow(2, FS));
                  ({Sign, Exponent, Field, Ubit}) ->
                      math:pow(-1, Sign)*math:pow(2, Exponent-math:pow(2, ES-1)-1)*(1+(Field/math:pow(2, FS)))
              end,
        {reply, {View, U2F(View)}, State}
    catch
        error:Error -> {reply, {error, Error, State}, State}
    end;
handle_call(_,_,State) ->
    {reply, State, State}.

set(Pid, field_size, Value) ->
    gen_server:cast(Pid, {set, field_size, Value});
set(Pid, exponent_size, Value) ->
    gen_server:cast(Pid, {set, exponent_size, Value}).

bitstring(Pid, Bitstring) ->
    gen_server:call(Pid, {bitstring, Bitstring}).


decode(Bitstring, {FSS, EFSS}) ->
    <<TEFS:EFSS, TFS:FSS, Rest/bitstring>> = Bitstring,
    EFS = TEFS+1,
    FS = TFS+1,
    MaxSize = math:pow(2,EFS) + math:pow(2,FS) + 2,
    <<Ubit:1, Field:FS, Exponent:EFS, Sign:1>> = Rest,
    #{ max_size => MaxSize
     , exponent_field_size_size => EFSS
     , field_size_size => FSS
     , exponent_field_size => EFS
     , field_size => FS
     , ubit => Ubit
     , field => Field
     , exponent => Exponent
     , sign => Sign
     }.

reverse(Bitstring) ->
    reverse(Bitstring, <<>>).

reverse(<<>>, Buffer) ->
    Buffer;
reverse(<<B:1,R/bitstring>>, Buffer) ->
    reverse(R, <<B:1, Buffer/bitstring>>).

reverse2(Bitstring) ->
    reverse2(Bitstring, <<>>).

reverse2(<<>>, Buffer) ->
    Buffer;
reverse2(<<B/integer, R/bitstring>>, Buffer) ->
    BB = B bxor 255,
    reverse2(R, <<BB, Buffer/bitstring>>).
