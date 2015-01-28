-module(wechater_login_fsm).
-behaviour(gen_fsm).

-export([start_link/0, init/1, handle_info/3]).

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
    

fetch_wx_login_page(Profile = #wx_profile{requests = Requests}) ->
    case lists:keyfind(main_page, 1, Requests) of
        Id -> httpc:cancel_request(Id);
        false -> ok
    end,
    {ok, RequestId} = httpc:request(get, {"https://wx.qq.com", []}, [], [{sync, false}]),
    NewRequests = lists:keystore(main_page, 1, Requests, {main_page, RequestId}),
    
    Profile#wx_profile{requests = NewRequests}.

fetch_wx_uuid(Appid, Profile) ->
    ok.

poll_wx_login_status(Profile) ->
    ok.

handle_info({http, {RequestId, Result}}, fetch_login_page
           , Profile = #wx_profile{requests = Requests}) ->
    NewRequests = lists:keydelete(RequestId, 2, Requests),
    NewProfile = Profile#wx_profile{requests = NewRequests},
    case Result of
        {ok, {_, _, B}} ->
            fetch_login_page({get_uuid, parse_appid(B)}
                            , NewProfile);
        _ -> new_login(NewProfile)
    end.

parse_appid(Html) ->
    ok.

parse_loginjs(Html) ->
    Tree = mochiweb_html:parse(Html),
    {_, Attr, _} = tree_find(fun({N, A, C}) ->
                                     case N of
                                         <<"script">> ->
                                             case lists:keyfind(<<"src">>, 1, A) of
                                                 {K, V} ->
                                                     case re:run(V, "login[0-9]+.js") of
                                                         {match, _} ->
                                                             true;
                                                         _ -> false
                                                     end;
                                                 _ -> false
                                             end;
                                         _ -> false
                                     end
                             end, Tree),
    {<<"src">>, Js} = lists:keyfind(<<"src">>, 1, Attr),
    binary_to_list(Js).

tree_find(Func, {N, A, []}) ->
    case Func({N, A, []}) of
        true ->
            {N, A, []};
        _ ->
            not_found
    end;

tree_find(Func, Tree = {N, A, [C|Cs]}) ->
    case Func(Tree) of
        true ->
            Tree;
        _ ->
            if is_tuple(C) ->
                    case tree_find(Func, C) of
                        not_found ->
                            tree_find(Func, Cs);
                        Node -> Node
                    end;
               true ->
                    tree_find(Func, Cs)
            end
    end;
tree_find(Func, {N,_}) ->
    not_found;

tree_find(Func, [N|Ns]) ->
    if is_tuple(N) ->
            case tree_find(Func, N) of
                not_found ->
                    tree_find(Func, Ns);
                Node ->
                    Node
            end;
       true ->
            tree_find(Func, Ns)
    end;

tree_find(Func, []) ->
    not_found.



-ifdef(TEST).
parse_appid_test() ->
    {ok, Dir} = file:get_cwd(),
    erlang:display(Dir),
    {ok, Html} = file:read_file("../test/fixtures/login.html"),
    ?assertEqual("wx782c26e4c19affb", parse_appid(Html)).

parse_loginjs_test() ->
    {ok, Html} = file:read_file("../test/fixtures/login.html"),
    ?assertEqual("https://res.wx.qq.com/zh_CN/htmledition/js/login214065.js", parse_loginjs(Html)).
-endif.
