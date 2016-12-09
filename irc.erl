-module(irc).
-export([start_worker/2, work/3, work/4]).
-include("conf.hrl").

start_worker(Name, Conf) ->
    Host = Conf#conf.host,
    Port = Conf#conf.port,
    io:fwrite("{~s} Connecting to ~s:~w~n", [Name, Host, Port]),
	{ok, Sock} = connect(Host, Port),
    io:fwrite("{~s} Established TCP connection~n", [Name]),
    work(Name, Sock, Conf),
	Sock.

connect(Host, Port) ->
	{ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
	{ok, Sock}.

pid_hash() ->
    Raw = binary_to_list(erlang:md5(integer_to_list(os:system_time())++pid_to_list(self()))),
    lists:flatten(lists:map(fun(X) -> io_lib:format("~.16b",[X]) end, Raw)).

send(Sock, Command, Args) ->
    Line = Command++" "++Args++"\r\n",
    io:fwrite("(~~>) ~s", [Line]),
    ok = gen_tcp:send(Sock, Line),
    ok.


handle(Nick, Chan, Msg) ->
    case string:equal(".choose ", string:left(Msg, 8) of
        true ->
            Choices = lists:map(fun string:strip/1, string:tokens(string:substr(Msg, 9), ",")),
            Index = rand:uniform(string:len(Choices)),
            Result = lists:nth(Index, Choices),
            self() ! {privmsg, Chan, Nick++": "++Result};
        false ->
            ok
    end,
    CmdPfx = Nick++":",
    CmdStart = string:len(CmdPfx),
    case string:equal(CmdPfx, string:left(Msg, CmdStart)) of
        true ->
            Command = string:strip(string:substr(Msg, CmdStart+1), left),
            case Command of
                 "die" ->
                       self() ! {quit, "Obediently committed sudoku"};
                 "nodes" ->
                       Nodes = lists:flatten(io_lib:format("~w", [nodes()])),
                       netsv ! {send_privmsg, Chan, Nodes};
                 "spawn" ->
                       netsv ! {spawn};
                 "upgrade" ->
                       supervisor ! {upgrade};
                 "join "++Targets ->
                       supervisor ! {send_join, Targets};
                 "say "++Something ->
                       supervisor ! {send_privmsg, Chan, Something};
                 Other ->
                       io:fwrite("Unhandled command ~s~n", [Other]),
                       ok
            end,
            io:fwrite("'~s' len: ~w~n", [Command, string:len(Command)]);
        false ->
            ok
    end.

dispatch(Sock, Conf, Nick, Line) ->
    %io:fwrite("(~~<) ~s", [Line]),
    case string:to_upper(string:sub_word(Line, 1)) of
        "PING" ->
            Id = lists:droplast(lists:droplast(string:substr(Line, 6))),
            send(Sock, "PONG", Id);
        "ERROR" ->
            ok;
        ":"++Prefix ->
            case string:to_upper(string:sub_word(Line, 2)) of
                "001" ->
                    self() ! {join, Conf#conf.chan},
                    ok;
                "PRIVMSG" ->
                    io:fwrite("() ~s", [Prefix]),
                    case Prefix of
                        "earl-"++_Rest ->
                            ignore;
                        _ ->
                            supervisor ! {forward, Line},
                            Chan = string:sub_word(Line, 3),
                            Message__ = lists:dropwhile(fun(X) -> X /= $\s end, string:substr(Line, 1)),
                            Message_ = lists:dropwhile(fun(X) -> X /= $: end, Message__),
                            Message = lists:droplast(lists:droplast(string:substr(Message_, 2))),
                            handle(Nick, Chan, Message)
                    end,
                    ok;
                "KICK" ->
                    self() ! {join, "#programming"},
                    ok;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

work(Name, Sock, Conf) ->
    work(Name, Sock, Conf, ok).
work(Name, Sock, Conf, Nick) ->
    receive
        {register} ->
            Hash = string:substr(pid_hash(), 1, 5),
            Nick_ = string:substr("earl-"++Hash, 1, 10),
            ok = send(Sock, "USER", "earl * * :Earl node"),
            ok = send(Sock, "NICK", Nick_),
            work(Name, Sock, Conf, Nick_);
        {join, Chan} ->
            ok = send(Sock, "JOIN", Chan),
            work(Name, Sock, Conf, Nick);
        {privmsg, Chan, Msg} ->
            ok = send(Sock, "PRIVMSG", Chan++" :"++Msg),
            work(Name, Sock, Conf, Nick);
        {quit, Msg} ->
            ok = send(Sock, "QUIT", ":"++Msg),
            gen_tcp:close(Sock),
            ok;
        {reinstance} ->
            io:fwrite("{~s} Reinstancing.~n", [Name]),
            ?MODULE:work(Name, Sock, Conf, Nick);
        {upgrade} ->
            io:fwrite("{~s} Upgrading.~n", [Name]),
            try
                code:purge(?MODULE),
                code:delete(?MODULE),
                compile:file(?MODULE),
                code:load_file(?MODULE),
                netsv ! {reinstance}
            catch
                _:_ ->
                    io:fwrite("{~s} Upgrade failed.~n", [Name])
            end,
            work(Name, Sock, Conf, Nick);
        {send, Command, Args} ->
            ok = send(Sock, Command, Args),
            work(Name, Sock, Conf, Nick);
        {tcp_closed, _} ->
            io:fwrite("{~s} Lost TCP connection. Finishing~n", [Name]),
            ok;
        {tcp, Sock, Line} ->
            ok = dispatch(Sock, Conf, Nick, Line),
            work(Name, Sock, Conf, Nick);
        Other ->
            io:fwrite("{~s} Ignoring undefined message ~w~n", [Name, Other]),
            work(Name, Sock, Conf, Nick)
    end.
