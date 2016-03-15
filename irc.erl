-module(irc).
-export([start_worker/3, work/2]).

start_worker(Name, Host, Port) ->
    io:fwrite("{~s} Connecting to ~s:~w~n", [Name, Host, Port]),
	{ok, Sock} = connect(Host, Port),
    io:fwrite("{~s} Established TCP connection~n", [Name]),
    work(Name, Sock),
	Sock.

connect(Host, Port) ->
	{ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
	{ok, Sock}.

pid_hash() ->
    Raw = binary_to_list(erlang:md5(pid_to_list(self()))),
    lists:map(fun(X) -> io_lib:format("~.16b",[X]) end, Raw).

send(Sock, Command, Args) ->
    Line = Command++" "++Args++"\r\n",
    io:fwrite("(~~>) ~s", [Line]),
    ok = gen_tcp:send(Sock, Line),
    ok.

dispatch(Sock, Line) ->
    io:fwrite("(~~<) ~s", [Line]),
    case string:to_upper(string:sub_word(Line, 1)) of
        "PING" ->
            Id = lists:droplast(lists:droplast(string:substr(Line, 6))),
            send(Sock, "PONG", Id);
        "ERROR" ->
            ok;
        ":"++_Prefix ->
            case string:to_upper(string:sub_word(Line, 2)) of
                "001" ->
                    self() ! {join, "#programming"},
                    ok;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

work(Name, Sock) ->
    receive
        {register} ->
            Hash = string:substr(pid_hash(), 1, 5),
            ok = send(Sock, "USER", "earl * * :Earl node"),
            ok = send(Sock, "NICK", "earl-"++Hash),
            work(Name, Sock);
        {join, Chan} ->
            ok = send(Sock, "JOIN", Chan),
            work(Name, Sock);
        {privmsg, Chan, Msg} ->
            ok = send(Sock, "PRIVMSG", Chan++" :"++Msg),
            work(Name, Sock);
        {quit, Msg} ->
            ok = send(Sock, "QUIT", ":"++Msg),
            gen_tcp:close(Sock),
            ok;
        {upgrade} ->
            io:fwrite("{~s} Upgrading.~n", [Name]),
            ?MODULE:work(Name, Sock);
        {send, Command, Args} ->
            ok = send(Sock, Command, Args),
            work(Name, Sock);
        {tcp_closed, _} ->
            io:fwrite("{~s} Lost TCP connection. Finishing~n", [Name]),
            ok;
        {tcp, Sock, Line} ->
            ok = dispatch(Sock, Line),
            work(Name, Sock);
        Other ->
            io:fwrite("{~s} Ignoring undefined message ~w~n", [Name, Other]),
            work(Name, Sock)
    end.
