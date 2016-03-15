-module(svcsv).
-export([init/1, svloop/2]).
-include("conf.hrl").

svloop(Conf, [ConPid, NetPid, ProxyPid]) ->
    receive
        {upgrade} ->
            io:format("{svcsv} Upgrading.~n"),
            netsv ! {upgrade},
            ?MODULE:svloop(Conf, [ConPid, NetPid, ProxyPid]);
        {'EXIT', _, normal} ->
            ok;
        {'EXIT', _, shutdown} ->
            ok;
        {'EXIT', ConPid, Other} ->
            io:fwrite("{svcsv} Console process ~w ended abnormally with code: ~w~n", [ConPid, Other]),
            ConPid_ = spawn_link(con, open, []),
            svloop(Conf, [ConPid_, NetPid, ProxyPid]);
        {'EXIT', NetPid, Other} ->
            io:fwrite("{svcsv} Networking process ~w ended abnormally with code: ~w~n", [NetPid, Other]),
            NetPid_ = spawn_link(netsv, boot, [Conf]),
            svloop(Conf, [ConPid, NetPid_, ProxyPid]);
        {'EXIT', ProxyPid, Other} ->
            io:fwrite("{proxy} Client proxy ~w ended abnormally with code: ~w~n", [ProxyPid, Other]),
            ProxyPid_ = spawn_link(proxy, open, [Conf#conf.port]),
            svloop(Conf, [ConPid, NetPid, ProxyPid_]);
        Other ->
            io:fwrite("{svcsv} Ignoring undefined message ~w~n", [Other]),
            svloop(Conf, [ConPid, NetPid, ProxyPid])
    end.

wait_con() ->
    receive
        {con, ready} ->
            ok 
    after
        10000 ->
            erlang:error("Timed out waiting for a console")
    end.

wait_netsv() ->
    receive
        {netsv, ready} ->
            ok 
    after
        10000 ->
            erlang:error("Timed out waiting for network supervisor")
    end.

wait_proxy() ->
    receive
        {proxy, ready} ->
            ok 
    after
        10000 ->
            erlang:error("Timed out waiting for client proxy")
    end.

init(Conf) ->
    process_flag(trap_exit, true),
    register(svcsv, self()),
    io:format("SVCSV: Initializing Services~n"),
    ConPid = spawn_link(con, open, []),
    NetPid = spawn_link(netsv, boot, [Conf]),
    ProxyPid = spawn_link(proxy, open, [Conf#conf.port]),
    wait_con(),
    wait_netsv(),
    wait_proxy(),
    appsv ! {svcsv, ready},
    svloop(Conf, [ConPid, NetPid, ProxyPid]),
    io:format("SVCSV: Leaving~n").
