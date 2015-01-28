-module(wechater_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    httpc:set_options([{cookies, enabled}
                      , {proxy, case os:getenv("http_proxy") of
                                    false -> {undefined, []};
                                    Proxy -> {Proxy, ["localhost"]}
                                end}]),
    wechater_sup:start_link().

stop(_State) ->
    ok.
