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
fetch_login_page({get_js, JsUrl}, Profile) ->
    NewProfile = fetch_url(JsUrl, login_js, Profile),
    {next_state, fetch_login_js, NewProfile}.

fetch_login_js({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
fetch_login_js({get_uuid, Appid}, Profile) ->
    Url = "https://login.wx.qq.com/jslogin?appid=" ++ Appid 
        ++ "&redirect_uri=&redirect_uri=https%3A%2F%2Fwx.qq.com%2Fcgi-bin%2Fmmwebwx-bin%2Fwebwxnewloginpage&fun=new&lang=en_US&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, uuid, Profile),
    {next_state, fetch_uuid, NewProfile}.

fetch_uuid({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
fetch_uuid({poll_login, Uuid}, Profile) ->
    Url = "https://login.wx.qq.com/cgi-bin/mmwebwx-bin/login?uuid=" ++ Uuid
        ++ "&tip=1&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, login_status, Profile),
    {next_state, poll_login, NewProfile#wx_profile{uuid = Uuid}}.

poll_login({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
poll_login(succeed, Profile) ->
    {next_state, succeed, Profile};
poll_login(not_yet, Profile = #wx_profile{uuid = Uuid}) ->
    Url = "https://login.wx.qq.com/cgi-bin/mmwebwx-bin/login?uuid=" ++ Uuid
        ++ "&tip=1&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, login_status, Profile),
    {next_state, poll_login, NewProfile};
poll_login(viewed, Profile = #wx_profile{uuid = Uuid}) ->
    Url = "https://login.wx.qq.com/cgi-bin/mmwebwx-bin/login?uuid=" ++ Uuid
        ++ "&tip=0&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, login_status, Profile),
    {next_state, poll_login, NewProfile}.

suceed(restart, Profile) ->
    new_login(Profile).

new_login(Profile) ->
    NewProfile = fetch_url("https://wx.qq.com", login_page, Profile),
    Timer = gen_fsm:start_timer(5 * 60 * 1000, []),
    {next_state, fetch_login_page, NewProfile#wx_profile{timer = Timer
                                                        , uuid = undefined}}.
    
fetch_url(Url, Ref, Profile = #wx_profile{requests = Requests}) ->
    case lists:keyfind(Ref, 1, Requests) of
        {_, Id} -> httpc:cancel_request(Id);
        false -> ok
    end,
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    NewRequests = lists:keystore(Ref, 1, Requests, {Ref, RequestId}),
    
    Profile#wx_profile{requests = NewRequests}.

poll_wx_login_status(Profile) ->
    ok.

handle_info({http, Resp}, fetch_login_page, Profile) ->
    Succeed = fun(B, P) -> fetch_login_page({get_js, parse_loginjs(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_login_page, login_page, Succeed, Fail);

handle_info({http, Resp}, fetch_login_js, Profile) ->
    Succeed = fun(B, P) -> fetch_login_js({get_uuid, parse_appid(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_login_js, login_js, Succeed, Fail);
handle_info({http, Resp}, fetch_uuid, Profile) ->
    Succeed = fun(B, P) -> fetch_login_js({poll_login, parse_uuid(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_uuid, uuid, Succeed, Fail);
handle_info({http, Resp}, poll_login, Profile) ->
    Succeed = fun(B, P) -> poll_login(parse_login_status(B), P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, poll_login, login_status, Succeed, Fail).

handle_http_resp({RequestId, Result}, Profile, State, HttpRef, Succeed, Fail) ->
    #wx_profile{requests = Requests} = Profile,
    case lists:keyfind(RequestId, 2, Requests) of
        {HttpRef, RequestId} ->
            NewRequests = lists:keydelete(RequestId, 2, Requests),
            NewProfile = Profile#wx_profile{requests = NewRequests},
            case Result of
                {ok, {_, _, B}} ->
                    Succeed(B, NewProfile);
                Err -> Fail(Err, NewProfile)
            end;
        _ ->
            {next_state, State, Profile}
    end.

parse_login_status(Js) ->
    {match, [Code|_]} = re:run(Js, "window.code[\s]*=[\s]*([0-9]*);", [{capture, [1], list}]),
    case Code of
        "408" -> not_yet;
        "201" -> viewed;
        "200" -> succeed
    end.

parse_uuid(Js) ->
    {match, [Uuid|_]} = re:run(Js, "uuid[\s]*=[\s]*\"([a-z0-9]*)\";", [{capture, [1], list}]),
    Uuid.

parse_appid(Js) ->
    {match, [Appid|_]} = re:run(Js, "appid=([a-z0-9]*)", [{capture, [1], list}]),
    Appid.

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

timestamp() ->
    {S, MS, MS2} = now(),
    Sec = lists:flatten(io_lib:format("~w", [S])),
    MicroSec = lists:flatten(io_lib:format("~6..0w", [MS])),
    MillSec = lists:flatten(io_lib:format("~3..0w", [round(MS2/1000)])),
    Sec ++ MicroSec ++ MillSec.


-ifdef(TEST).
parse_appid_test() ->
    {ok, Html} = file:read_file("../test/fixtures/login.js"),
    ?assertEqual("wx782c26e4c19acffb", parse_appid(Html)).

parse_loginjs_test() ->
    {ok, Html} = file:read_file("../test/fixtures/login.html"),
    ?assertEqual("https://res.wx.qq.com/zh_CN/htmledition/js/login214065.js", parse_loginjs(Html)).

parse_uuid_test() ->
    {ok, Html} = file:read_file("../test/fixtures/uuid.js"),
    ?assertEqual("637ef49c37fc43", parse_uuid(Html)).
-endif.
