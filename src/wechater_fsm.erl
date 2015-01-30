-module(wechater_fsm).
-behaviour(gen_fsm).

-export([start_link/1, init/1, handle_info/3, code_change/4, handle_event/3, handle_sync_event/4, terminate/3]).

-record(wx_secret, {keys}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



start_link(Keys) ->
    Ret = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    gen_fsm:send_event(?MODULE, init),
    Ret.

init(_Args) ->
    Profile = #wx_profile{uuid = undefined
                         , cookie = []
                         , requests = []},
    {ok, not_start, Profile}.

code_change(_OldVsn, State, Profile, _Ext) ->
    {ok, State, Profile, _Ext}.

handle_event({timeout, _Ref, _Msg}, _State, Profile) ->
    {stop, timeout, Profile}.

handle_sync_event({timeout, _Ref, _Msg}, _From, _State, Profile) ->
    {stop, timeout, Profile}.

terminate(_Reason, _State, _Profile) ->
    ok.

not_start(init,
