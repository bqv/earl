-module(netsv).
-export([boot/1, netloop/3]).
-include("conf.hrl").

pick_worker(Workers) ->
    List = dict:fetch_keys(Workers),
    Index = rand:uniform(length(List)),
    Ref = lists:nth(Index, List),
    {Pid, Wname} = dict:fetch(Ref, Workers),
    {Pid, Wname}.

make_worker(Index, Workers, Conf) ->
    Wname = "net#" ++ integer_to_list(Index),
    io:fwrite("{netsv} Spawning Worker ~s~n", [Wname]),
    {Pid, Ref} = run_worker(Conf, Wname),
    dict:store(Ref, {Pid, Wname}, Workers).

netloop(Conf, Workers, NextN) ->
    receive
        {reinstance} ->
            dict:map(fun(_,{Pid,_}) -> Pid ! {reinstance} end, Workers),
            netloop(Conf, Workers, NextN);
        {upgrade} ->
            io:format("{netsv} Upgrading.~n"),
            {Pid, Wname} = pick_worker(Workers),
            Pid ! {upgrade},
            try
                code:purge(?MODULE),
                code:delete(?MODULE),
                compile:file(?MODULE),
                code:load_file(?MODULE),
                ?MODULE:netloop(Conf, Workers, NextN)
            catch
                A:B ->
                    io:fwrite("{netsv} Upgrade failed: ~w:~w.~n", [A,B]),
                    netloop(Conf, Workers, NextN)
            end;
        {send_join, Chans} ->
            io:fwrite("{netsv} Joining ~s.~n", [Chans]),
            dict:map(fun(_,{Pid,_}) -> Pid ! {send, "JOIN", ":"++Chans} end, Workers),
            netloop(Conf, Workers, NextN);
        {send_privmsg, Chan, Msg} ->
            io:format("{netsv} Selecting...~n"),
            {Pid, Wname} = pick_worker(Workers),
            io:fwrite("{netsv} Dispatching to Worker ~s~n", [Wname]),
            case Msg of
                "/quit" ->
                    Pid ! {quit, "command line quit hack"};
                _ ->
                    Pid ! {send, "PRIVMSG", Chan++" :"++Msg}
            end,
            netloop(Conf, Workers, NextN);
        {spawn} ->
            Workers_ = make_worker(NextN, Workers, Conf),
            Conf_ = Conf#conf{wCur = length(Workers_)},
            supervisor ! {count, length(Workers_)},
            netloop(Conf_, Workers_, NextN+1);
        {'DOWN', Ref, process, _Pid, Reason} ->
            {_, {_, Wname}} = dict:find(Ref, Workers),
            io:fwrite("{netsv} Worker ~s stopped with reason: ~w~n", [Wname, Reason]),
            Workers_ = dict:erase(Ref, Workers),
            case length(Workers_) < Conf#conf.wMin of
                true ->
                    Workers__ = make_worker(NextN, Workers_, Conf),
                    Conf_ = Conf#conf{wCur = length(Workers__)},
                    supervisor ! {count, length(Workers__)},
                    netloop(Conf_, Workers__, NextN+1);
                false ->
                    Conf_ = Conf#conf{wCur = length(Workers_)},
                    supervisor ! {count, length(Workers_)},
                    netloop(Conf_, Workers_, NextN)
            end;
        Other ->
            io:fwrite("{netsv} Ignoring undefined message ~w~n", [Other]),
            netloop(Conf, Workers, NextN) 
    end.

run_worker(Conf, Wname) ->
    {Pid, Ref} = spawn_monitor(irc, start_worker, [Wname, Conf]),
    register(list_to_atom(Wname), Pid),
    Pid ! {register},
    {Pid, Ref}.

boot(Conf) ->
    register(netsv, self()),
    Workers = boot(Conf#conf.wMin, Conf),
    supervisor ! {netsv, ready},
    supervisor ! {count, Conf#conf.wMin},
    netloop(Conf, dict:from_list(Workers), Conf#conf.wMin + 1).

boot(Count, Conf) when Count > 0 ->
    Wname = "net#" ++ integer_to_list(Count),
    io:fwrite("{netsv} Spawning Worker ~s~n", [Wname]),
    {Pid, Ref} = run_worker(Conf, Wname),
    [{Ref, {Pid, Wname}} | boot(Count - 1, Conf)];
boot(0, _) ->
    [].
