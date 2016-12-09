-module(con).
-export([open/0, read/0]).

open() ->
    io:format("{con} Opening Console~n"),
    supervisor ! {con, ready},
    read(),
    io:format("{con} Closing Console~n").

read() ->
    Data = lists:droplast(io:get_line('>\r')),
    case Data of
        ":q" ->
            io:format("{con} Quitting.~n");
        _ ->
            io:fwrite("{con} Got line '~s' from <stdin>~n", [Data]),
            netsv ! {send_privmsg, "#programming", Data},
            read()
    end.
