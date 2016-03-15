-module(netsv).
-export([boot/1, netloop/3]).
-include("conf.hrl").

pick_worker(Workers) ->
    List = dict:fetch_keys(Workers),
    Index = rand:uniform(length(List)),
    Ref = lists:nth(Index, List),
    {Pid, Wname} = dict:fetch(Ref, Workers),
    {Pid, Wname}.

netloop(Conf, Workers, NextN) ->
    receive
        {upgrade} ->
            io:format("{netsv} Upgrading.~n"),
            dict:map(fun(_,{Pid,_}) -> Pid ! {upgrade} end, Workers),
            ?MODULE:netloop(Conf, Workers, NextN);
        {send_join, Chans} ->
            io:fwrite("{netsv} Joining ~s.~n", [Chans]),
            dict:map(fun(_,{Pid,_}) -> Pid ! {send, "JOIN", ":"++Chans} end, Workers),
            netloop(Conf, Workers, NextN);
        {send_privmsg, Chan, Msg} ->
            {Pid, Wname} = pick_worker(Workers),
            io:fwrite("{netsv} Dispatching to Worker ~s~n", [Wname]),
            case Msg of
                "/quit" ->
                    Pid ! {quit, "command line quit hack"};
                _ ->
                    Pid ! {send, "PRIVMSG", Chan++" :"++Msg}
            end,
            netloop(Conf, Workers, NextN);
        {'DOWN', Ref, process, _Pid, Reason} ->
            {_, {_, Wname}} = dict:find(Ref, Workers),
            io:fwrite("{netsv} Worker ~s stopped with reason: ~w~n", [Wname, Reason]),
            Workers_ = dict:erase(Ref, Workers),
            Wname_ = "net#" ++ integer_to_list(NextN),
            io:fwrite("{netsv} Spawning Worker ~s~n", [Wname_]),
            {Pid_, Ref_} = run_worker(Conf, Wname_),
            Workers__ = dict:store(Ref_, {Pid_, Wname_}, Workers_),
            netloop(Conf, Workers__, NextN+1);
        Other ->
            io:fwrite("{netsv} Ignoring undefined message ~w~n", [Other]),
            netloop(Conf, Workers, NextN) 
    end.

run_worker(Conf, Wname) ->
    {Pid, Ref} = spawn_monitor(irc, start_worker, [Wname, Conf#conf.host, Conf#conf.port]),
    register(list_to_atom(Wname), Pid),
    Pid ! {register},
    {Pid, Ref}.

boot(Conf) ->
    register(netsv, self()),
    Workers = boot(Conf#conf.wMin, Conf),
    svcsv ! {netsv, ready},
    netloop(Conf, dict:from_list(Workers), Conf#conf.wMin + 1).

boot(Count, Conf) when Count > 0 ->
    Wname = "net#" ++ integer_to_list(Count),
    io:fwrite("{netsv} Spawning Worker ~s~n", [Wname]),
    {Pid, Ref} = run_worker(Conf, Wname),
    [{Ref, {Pid, Wname}} | boot(Count - 1, Conf)];
boot(0, _) ->
    [].
