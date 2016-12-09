-module(remsv).
-export([init/1, remloop/3]).
-include("conf.hrl").

remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid]) ->
    receive
        {upgrade} ->
            io:format("{remsv} Upgrading.~n"),
            netsv ! {upgrade},
            try
                code:purge(?MODULE),
                code:delete(?MODULE),
                compile:file(?MODULE),
                code:load_file(?MODULE),
                ?MODULE:remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid])
            catch
                _:_ ->
                    io:format("{remsv} Upgrade failed.~n"),
                    remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid])
            end;
        {'EXIT', _, normal} ->
            ok;
        {'EXIT', _, shutdown} ->
            ok;
        {'EXIT', ConPid, Other} ->
            io:fwrite("{remsv} Console process ~w ended abnormally with code: ~w~n", [ConPid, Other]),
            ConPid_ = spawn_link(con, open, []),
            remloop(Conf, SvcPid, [ConPid_, NetPid, ProxyPid]);
        {'EXIT', NetPid, Other} ->
            io:fwrite("{remsv} Networking process ~w ended abnormally with code: ~w~n", [NetPid, Other]),
            NetPid_ = spawn_link(netsv, boot, [Conf]),
            remloop(Conf, SvcPid, [ConPid, NetPid_, ProxyPid]);
        {'EXIT', ProxyPid, Other} ->
            io:fwrite("{proxy} Client proxy ~w ended abnormally with code: ~w~n", [ProxyPid, Other]),
            ProxyPid_ = spawn_link(proxy, open, [1838]),
            remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid_]);
        {forward, Line} ->
            ProxyPid ! {forward, Line},
            remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid]);
        {relay_join, Chans} ->
            netsv ! {send_join, Chans},
            remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid]);
        {relay_privmsg, Chan, Msg} ->
            netsv ! {send_privmsg, Chan, Msg},
            remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid]);
        {count, Count} ->
            SvcPid ! {ping, self(), Count},
            remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid]);
        Other ->
            io:fwrite("{remsv} Ignoring undefined message ~w~n", [Other]),
            remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid])
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

remote_link(Conf) ->
    io:fwrite("{remsv} Attempting to link to ~w~n", [Conf#conf.node]),
    true = net_kernel:connect(Conf#conf.node),
    io:fwrite("{remsv} Connected... sending ~w~n", [{link, self()}]),
    {supervisor, Conf#conf.node} ! {link, self()},
    receive
        {link, Pid} ->
            io:fwrite("{remsv} Recieved link from ~w~n", [Pid]),
            Pid 
    after
        10000 ->
            erlang:error("Timed out waiting for remote client")
    end.

init(Conf) ->
    process_flag(trap_exit, true),
    register(supervisor, self()),
    io:format("REMSV: Initializing Services~n"),
    io:fwrite("REMSV: Registered on ~w~n", [self()]),
    SvcPid = remote_link(Conf),
    ConPid = spawn_link(con, open, []),
    NetPid = spawn_link(netsv, boot, [Conf]),
    ProxyPid = spawn_link(proxy, open, [1838]),
    wait_con(),
    wait_netsv(),
    wait_proxy(),
    appsv ! {remsv, ready},
    remloop(Conf, SvcPid, [ConPid, NetPid, ProxyPid]),
    io:format("REMSV: Leaving~n").
