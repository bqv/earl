-module(proxy).
-export([open/1, serve/1]).

open(Port) ->
    io:format("{proxy} Opening proxy~n"),
    supervisor ! {proxy, ready},
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

send_raw(Sock, Line) ->
    io:fwrite("(<~~) ~s", [Line]),
    ok = gen_tcp:send(Sock, Line),
    ok.

send(Sock, Command, Args) ->
    Line = Command++" "++Args++"\r\n",
    ok = send_raw(Sock, Line),
    ok.

forward(Sock, Line, History) ->
    case lists:member(Line, History) of
        true ->
            History;
        false ->
            ok = send_raw(Sock, Line),
            [Line|lists:sublist(History,15)]
    end.

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
            supervisor ! {send_join, Chans};
        "PRIVMSG" ->
            Chan = string:sub_word(Line, 2),
            Msg = strip(string:substr(Line, 7+1+length(Chan)+1 +1)),
            io:fwrite(" [~s] ~s", [Chan, Msg]),
            supervisor ! {send_privmsg, Chan, Msg};
        _ ->
            ok
    end.

serve(Sock) ->
    serve(Sock, []).
serve(Sock, History) ->
    receive
        {upgrade} ->
            io:format("{proxy} Upgrading.~n"),
            ?MODULE:serve(Sock);
        {send, Command, Args} ->
            ok = send(Sock, Command, Args),
            serve(Sock, History);
        {forward, Line} ->
            NextHist = forward(Sock, Line, History),
            serve(Sock, NextHist);
        {tcp_closed, _} ->
            io:format("{proxy} Lost client. Opening new listener.~n"),
            gen_tcp:close(Sock, History),
            ok;
        {tcp, Sock, Line} ->
            dispatch(Sock, lists:droplast(lists:droplast(Line))),
            serve(Sock, History);
        Other ->
            io:fwrite("{proxy} Ignoring undefined message ~w~n", [Other]),
            serve(Sock, History)
    end.
