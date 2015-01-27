-module(wechater_login_fsm).
-behaviour(gen_fsm).

-export([start_link/0, init/1, handle_info/2]).

-export([not_start/2, fetch_login_page/2]).

-record(wx_profile, {uuid, cookie, timer, requests}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    gen_fsm:send_event(?MODULE, start).

init(_Args) ->
    Profile = #wx_profile{uuid = undefined
                         , cookie = []
                         , requests = []},
    {ok, not_start, Profile}.

not_start(start, Profile) ->
    new_login(Profile).

fetch_login_page({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);                                      
fetch_login_page({get_uuid, Appid}, Profile) ->
    NewProfile = fetch_wx_uuid(Appid, Profile),
    {next_state, fetch_uuid, NewProfile}.

fetch_uuid({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
fetch_uuid(poll_login, Profile) ->
    NewProfile = poll_wx_login_status(Profile),
    {next_state, poll_login, NewProfile}.

poll_login({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
poll_login(succeed, Profile) ->
    {next_state, succeed, Profile};
poll_login(not_yet, Profile) ->
    NewProfile = poll_wx_login_status(Profile),
    {next_state, poll_login, NewProfile};
poll_login(viewed, Profile) ->
    NewProfile = poll_wx_login_status(Profile),
    {next_state, poll_login, NewProfile}.

suceed(restart, Profile) ->
    new_login(Profile).


new_login(Profile) ->
    NewProfile = fetch_wx_login_page(Profile),
    Timer = gen_fsm:start_timer(5 * 60 * 1000, []),
    {next_state, fetch_login_page, NewProfile#wx_profile{timer = Timer}}.
    

fetch_wx_main_page(Profile) ->
    ok.

fetch_wx_uuid(Appid, Profile) ->
    ok.
