-module(wechater_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    httpc:set_options([{cookies, enabled}
                      , {proxy, get_proxy(os:getenv("http_proxy"))}]),
    wechater_sup:start_link().

start() ->
    application:start(wechater).

get_proxy(Proxy) ->
    case Proxy of
        false -> {undefined, []};
        _ ->
            case re:run(Proxy, "(http[s]?:\/\/)?([^:]*):([0-9]*)", [{capture, [2, 3], list}]) of
                {match, [Host, Port]} ->
                    {{Host, erlang:list_to_integer(Port)}, ["localhost"]};
                E -> 
                    erlang:display(E),
                    {undefined, []}
            end
    end.

-ifdef(TEST).
get_proxy_test() ->
    ?assertEqual({{"192.168.32.9", 8080}, ["localhost"]}, get_proxy("http://192.168.32.9:8080")).
-endif.    

stop(_State) ->
    ok.
