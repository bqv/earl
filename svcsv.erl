-module(svcsv).
-export([init/1, svloop/3]).
-include("conf.hrl").

svloop(Conf, [ConPid, NetPid, ProxyPid], Links) ->
    receive
        {link, Pid} ->
            io:fwrite("{svcsv} Recieved link from ~w~n", [Pid]),
            Pid ! {link, self()},
            Links_ = dict:store(Pid, 0, Links),
            svloop(Conf, [ConPid, NetPid, ProxyPid], Links_);
        {upgrade} ->
            io:format("{svcsv} Upgrading.~n"),
            netsv ! {upgrade},
            try
                code:purge(?MODULE),
                code:delete(?MODULE),
                compile:file(?MODULE),
                code:load_file(?MODULE),
                ?MODULE:svloop(Conf, [ConPid, NetPid, ProxyPid], Links)
            catch
                _:_ ->
                    io:format("{svcsv} Upgrade failed.~n"),
                    svloop(Conf, [ConPid, NetPid, ProxyPid], Links)
            end;
        {'EXIT', _, normal} ->
            ok;
        {'EXIT', _, shutdown} ->
            ok;
        {'EXIT', ConPid, Other} ->
            io:fwrite("{svcsv} Console process ~w ended abnormally with code: ~w~n", [ConPid, Other]),
            ConPid_ = spawn_link(con, open, []),
            svloop(Conf, [ConPid_, NetPid, ProxyPid], Links);
        {'EXIT', NetPid, Other} ->
            io:fwrite("{svcsv} Networking process ~w ended abnormally with code: ~w~n", [NetPid, Other]),
            NetPid_ = spawn_link(netsv, boot, [Conf]),
            svloop(Conf, [ConPid, NetPid_, ProxyPid], Links);
        {'EXIT', ProxyPid, Other} ->
            io:fwrite("{proxy} Client proxy ~w ended abnormally with code: ~w~n", [ProxyPid, Other]),
            ProxyPid_ = spawn_link(proxy, open, [1838]),
            svloop(Conf, [ConPid, NetPid, ProxyPid_], Links);
        {forward, Line} ->
            ProxyPid ! {forward, Line},
            svloop(Conf, [ConPid, NetPid, ProxyPid], Links);
        {send_join, Chans} ->
            netsv ! {send_join, Chans},
            io:fwrite("{svcsv} Forawrding join to remote services ~p~n", [Links]),
            dict:map(fun(Pid, _) -> Pid ! {relay_join, Chans} end, Links),
            svloop(Conf, [ConPid, NetPid, ProxyPid], Links);
        {send_privmsg, Chan, Msg} ->
            TotalWorkers = dict:fold(fun(_,V,A) -> A+V end, Conf#conf.wCur, Links),
            Index = rand:uniform(TotalWorkers),
            case Index =< Conf#conf.wCur of
                true ->
                    netsv ! {send_privmsg, Chan, Msg};
                false ->
                    io:fwrite("{svcsv} Forawrding privmsg to remote services ~w~n", [Links]),
                    {Remote, _} = dict:fold(fun(K,V,{R, A}) ->
                                                case (V >= A) and (A >= 0) of
                                                    true -> {K, -1};
                                                    false -> {R, A-V}
                                                end
                                            end, {self(), Index - Conf#conf.wCur}, Links),
                    Remote ! {relay_privmsg, Chan, Msg}
            end,
            svloop(Conf, [ConPid, NetPid, ProxyPid], Links);
        {count, Count} ->
            io:fwrite("{svcsv} Booking at ~w workers~n", [Count]),
            Conf_ = Conf#conf{wCur = Count},
            svloop(Conf_, [ConPid, NetPid, ProxyPid], Links);
        {ping, Pid, Count} ->
            io:fwrite("{svcsv} Got ping from ~w with ~w workers~n", [Pid, Count]),
            Links_ = dict:store(Pid, Count, Links),
            svloop(Conf, [ConPid, NetPid, ProxyPid], Links_);
        Other ->
            io:fwrite("{svcsv} Ignoring undefined message ~w~n", [Other]),
            svloop(Conf, [ConPid, NetPid, ProxyPid], Links)
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
    register(supervisor, self()),
    io:format("SVCSV: Initializing Services~n"),
    io:fwrite("SVCSV: Registered on ~w~n", [self()]),
    Links = dict:from_list([]),
    ConPid = spawn_link(con, open, []),
    NetPid = spawn_link(netsv, boot, [Conf]),
    ProxyPid = spawn_link(proxy, open, [1838]),
    wait_con(),
    wait_netsv(),
    wait_proxy(),
    appsv ! {svcsv, ready},
    svloop(Conf, [ConPid, NetPid, ProxyPid], Links),
    io:format("SVCSV: Leaving~n").
