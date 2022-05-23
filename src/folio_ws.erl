-module(folio_ws).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {mod, mod_state}).

init(Req, State = #{module := Mod}) ->
    ?LOG_INFO(#{
        what => "New websocket connection",
        state => State
    }),
    ModState = Mod:init(),
    State1 = #state{mod = Mod, mod_state = ModState},

    {cowboy_websocket, Req, State1}.

websocket_handle(_Frame = {text, Text}, State = #state{mod = Mod, mod_state = MS}) ->
    Data = jsx:decode(Text, [return_maps]),

    ?LOG_INFO(#{
        what => "handle frame",
        frame => Data
    }),

    What = maps:get(<<"what">>, Data),
    Resp = Mod:handle_data(What, Data, MS),
    handle_resp(Resp, State);
websocket_handle(Frame, State) ->
    ?LOG_INFO(#{
        what => "non-text frame",
        frame => Frame
    }),
    {ok, State}.

websocket_info(Info, State = #state{mod = Mod, mod_state = MS}) ->
    Resp = Mod:handle_info(Info, MS),
    handle_resp(Resp, State);
websocket_info(Info, State) ->
    ?LOG_INFO(#{
        what => "handle info",
        info => Info
    }),
    {ok, State}.

handle_resp(Resp, State) ->
    case Resp of
        {ok, NewMS} ->
            {ok, State#state{mod_state = NewMS}};
        {reply, {to_json, ReplyData}, NewMS} ->
            Msg = jsx:encode(ReplyData),
            {reply, {text, Msg}, State#state{mod_state = NewMS}};
        {reply, Msg, NewMS} ->
            {reply, Msg, State#state{mod_state = NewMS}}
    end.
