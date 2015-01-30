-module(wechater_login_fsm).
-behaviour(gen_fsm).

-export([start_link/0, init/1, handle_info/3, code_change/4, handle_event/3, handle_sync_event/4, terminate/3]).

-export([not_start/2, fetch_login_page/2, fetch_login_js/2, fetch_uuid/2, poll_login/2, succeed/2, fetch_login_key/2]).

-record(wx_profile, {uuid, keys, cookie, timer = undefined, requests}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


start_link() ->
    Ret = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    gen_fsm:send_event(?MODULE, start),
    Ret.

init(_Args) ->
    Profile = #wx_profile{uuid = undefined
                         , cookie = []
                         , requests = []},
    {ok, not_start, Profile}.

code_change(_OldVsn, State, Profile, _Ext) ->
    {ok, State, Profile, _Ext}.

handle_event({timeout, _Ref, _Msg}, _State, Profile) ->
    new_login(Profile).

handle_sync_event({timeout, _Ref, _Msg}, _From, _State, Profile) ->
    new_login(Profile).

terminate(_Reason, _State, _Profile) ->
    ok.
    
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
    Url = "https://login.weixin.qq.com/jslogin?appid=" ++ Appid 
        ++ "&redirect_uri=&redirect_uri=https%3A%2F%2Fwx.qq.com%2Fcgi-bin%2Fmmwebwx-bin%2Fwebwxnewloginpage&fun=new&lang=en_US&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, uuid, Profile),
    {next_state, fetch_uuid, NewProfile}.

fetch_uuid({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
fetch_uuid({poll_login, Uuid}, Profile) ->
    Url = "https://login.weixin.qq.com/cgi-bin/mmwebwx-bin/login?uuid=" ++ Uuid
        ++ "&tip=1&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, login_status, Profile),
    {next_state, poll_login, NewProfile#wx_profile{uuid = Uuid}}.

poll_login({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
poll_login({succeed, RedirectUrl}, Profile) ->
    NewProfile = fetch_url(RedirectUrl, login_key, Profile),
    {next_state, fetch_login_key, NewProfile};
poll_login(not_yet, Profile = #wx_profile{uuid = Uuid}) ->
    Url = "https://login.weixin.qq.com/cgi-bin/mmwebwx-bin/login?uuid=" ++ Uuid
        ++ "&tip=1&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, login_status, Profile),
    {next_state, poll_login, NewProfile};
poll_login(viewed, Profile = #wx_profile{uuid = Uuid}) ->
    Url = "https://login.weixin.qq.com/cgi-bin/mmwebwx-bin/login?uuid=" ++ Uuid
        ++ "&tip=0&_=" ++ timestamp(),
    NewProfile = fetch_url(Url, login_status, Profile),
    {next_state, poll_login, NewProfile}.

fetch_login_key({timeout, _Ref, _Msg}, Profile) ->
    new_login(Profile);
fetch_login_key({keys, Keys}, Profile) ->
    NewProfile = cancel_timer(Profile),
    {next_state, succeed, NewProfile#wx_profile{keys = Keys}}.

succeed(restart, Profile) ->
    new_login(Profile).

new_login(Profile) ->
    NewProfile = fetch_url("https://wx.qq.com", login_page, cancel_timer(Profile)),
    Timer = gen_fsm:start_timer(5 * 60 * 1000, []),
    {next_state, fetch_login_page, NewProfile#wx_profile{timer = Timer
                                                        , uuid = undefined}}.
    
fetch_url(Url, Ref, Profile = #wx_profile{requests = Requests}) ->
    erlang:display("Fetching " ++ Url),
    case lists:keyfind(Ref, 1, Requests) of
        {_, Id} -> httpc:cancel_request(Id);
        false -> ok
    end,
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    NewRequests = lists:keystore(Ref, 1, Requests, {Ref, RequestId}),
    
    Profile#wx_profile{requests = NewRequests}.

handle_info({http, Resp}, fetch_login_page, Profile) ->
    Succeed = fun(B, P) -> fetch_login_page({get_js, parse_loginjs(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_login_page, login_page, Succeed, Fail);

handle_info({http, Resp}, fetch_login_js, Profile) ->
    Succeed = fun(B, P) -> fetch_login_js({get_uuid, parse_appid(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_login_js, login_js, Succeed, Fail);
handle_info({http, Resp}, fetch_uuid, Profile) ->
    Succeed = fun(B, P) -> fetch_uuid({poll_login, parse_uuid(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_uuid, uuid, Succeed, Fail);
handle_info({http, Resp}, poll_login, Profile) ->
    Succeed = fun(B, P) -> poll_login(parse_login_status(B), P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, poll_login, login_status, Succeed, Fail);
handle_info({http, Resp}, fetch_login_key, Profile) ->
    Succeed = fun(B, P) -> fetch_login_key({keys, parse_login_key(B)}, P) end,
    Fail = fun(_, P) -> new_login(P) end,
    handle_http_resp(Resp, Profile, fetch_login_key, login_key, Succeed, Fail).

handle_http_resp({RequestId, Result}, Profile, State, HttpRef, Succeed, Fail) ->
    #wx_profile{requests = Requests} = Profile,
    case lists:keyfind(RequestId, 2, Requests) of
        {HttpRef, RequestId} ->
            NewRequests = lists:keydelete(RequestId, 2, Requests),
            NewProfile = Profile#wx_profile{requests = NewRequests},
            case Result of
                {{_, _, "OK"}, _, B} ->
                    Succeed(B, NewProfile);
                Err -> Fail(Err, NewProfile)
            end;
        _ ->
            {next_state, State, Profile}
    end.

cancel_timer(Profile = #wx_profile{timer = Timer}) ->
    case Timer of
        undefined ->
            Profile;
        _ -> gen_fsm:cancel_timer(Timer),
             Profile#wx_profile{timer = undefined}
    end.

parse_login_key(Xml) ->
    Tree = mochiweb_html:parse(Xml),
    {_, _, [Skey]} = tree_find(fun({N, _, _}) -> N =:= <<"skey">> end, Tree),
    {_, _, [Wxsid]} = tree_find(fun({N, _, _}) -> N =:= <<"wxsid">> end, Tree),
    {_, _, [Wxuin]} = tree_find(fun({N, _, _}) -> N =:= <<"wxuin">> end, Tree),
    {_, _, [Ticket]} = tree_find(fun({N, _, _}) -> N =:= <<"pass_ticket">> end, Tree),
    [{skey, binary_to_list(Skey)}, {wxsid, binary_to_list(Wxsid)},
     {wxuin, binary_to_list(Wxuin)}, {ticket, binary_to_list(Ticket)}].
    
parse_login_status(Js) ->
    {match, [Code]} = re:run(Js, "window.code[\s]*=[\s]*([0-9]*);", [{capture, [1], list}]),
    case Code of
        "408" -> not_yet;
        "201" -> viewed;
        "200" -> 
            {match, [Ticket,Scan]} = re:run(Js, "ticket=([0-9a-z]*).*scan=([0-9]*)", [{capture, [1,2], list}]),
            {succeed, "https://wx.qq.com/cgi-bin/mmwebwx-bin/webwxnewloginpage?ticket=" ++ Ticket
             ++ "&lang=en_US&scan=" ++ Scan ++ "&fun=new"}
    end.

parse_uuid(Js) ->
    {match, [Uuid|_]} = re:run(Js, "uuid[\s]*=[\s]*\"([a-z0-9]*)\";", [{capture, [1], list}]),
    erlang:display("Get uuid " ++ Uuid),
    Uuid.

parse_appid(Js) ->
    {match, [Appid|_]} = re:run(Js, "appid=([a-z0-9]*)", [{capture, [1], list}]),
    Appid.

parse_loginjs(Html) ->
    Tree = mochiweb_html:parse(Html),
    {_, Attr, _} = tree_find(fun({N, A, _C}) ->
                                     case N of
                                         <<"script">> ->
                                             case lists:keyfind(<<"src">>, 1, A) of
                                                 {_K, V} ->
                                                     case re:run(V, "login[0-9a-z]+\.js") of
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

tree_find(Func, Tree = {_N, _A, [C|Cs]}) ->
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
tree_find(_Func, {_N,_}) ->
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

tree_find(_Func, []) ->
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

parse_login_key_test() ->
    {ok, Html} = file:read_file("../test/fixtures/login_key.xml"),
    Keys = parse_login_key(Html),
    ?assertEqual({wxsid, "gE5rOlqivQYi0Mw2"}, lists:keyfind(wxsid, 1, Keys)).

-endif.
