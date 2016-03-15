-module(proxy).
-export([open/1, serve/1]).

open(Port) ->
    io:format("{proxy} Opening proxy~n"),
    svcsv ! {proxy, ready},
    {ok, Sock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    listen(Sock),
    ok.

listen(Handle) ->
    {ok, Sock} = gen_tcp:accept(Handle),
    ok = serve(Sock),
    listen(Handle).

strip(String) ->
    case String of
        ":"++Content ->
            Content;
        Content ->
            Content
    end.

send(Sock, Command, Args) ->
    Line = Command++" "++Args++"\r\n",
    io:fwrite("(<~~) ~s", [Line]),
    ok = gen_tcp:send(Sock, Line),
    ok.

dispatch(Sock, Line) ->
    io:fwrite("(>~~) ~s", [Line]),
    case string:to_upper(string:sub_word(Line, 1)) of
        "PING" ->
            Id = string:substr(Line, 4+1 +1),
            send(Sock, "PONG", Id);
        "USER" ->
            send(Sock, ":earl!"++atom_to_list(node()), "001 $earl$ :earl");
        "JOIN" ->
            Chans = strip(string:substr(Line, 4+1 +1)),
            send(Sock, ":earl!"++atom_to_list(node()), "JOIN :"++Chans),
            netsv ! {send_join, Chans};
        "PRIVMSG" ->
            Chan = string:sub_word(Line, 2),
            Msg = strip(string:substr(Line, 7+1+length(Chan)+1 +1)),
            io:fwrite(" [~s] ~s", [Chan, Msg]),
            netsv ! {send_privmsg, Chan, Msg};
        _ ->
            ok
    end.

serve(Sock) ->
    receive
        {upgrade} ->
            io:format("{proxy} Upgrading.~n"),
            ?MODULE:serve(Sock);
        {send, Command, Args} ->
            ok = send(Sock, Command, Args),
            serve(Sock);
        {tcp_closed, _} ->
            io:format("{proxy} Lost client. Opening new listener.~n"),
            gen_tcp:close(Sock),
            ok;
        {tcp, Sock, Line} ->
            dispatch(Sock, lists:droplast(lists:droplast(Line))),
            serve(Sock);
        Other ->
            io:fwrite("{proxy} Ignoring undefined message ~w~n", [Other]),
            serve(Sock)
    end.
