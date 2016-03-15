-module(con).
-export([open/0, read/0]).

open() ->
    io:format("{con} Opening Console~n"),
    svcsv ! {con, ready},
    read(),
    io:format("{con} Closing Console~n").

upgrade([Mod|Modules]) ->
    code:purge(Mod),
    compile:file(Mod),
    code:load_file(Mod),
    upgrade(Modules);
upgrade([]) ->
    ok.

read() ->
    Data = lists:droplast(io:get_line('>\r')),
    case Data of
        ":q" ->
            io:format("{con} Quitting.~n");
        ":u" ->
            io:format("{con} Upgrading.~n"),
            upgrade([irc, netsv, proxy, svcsv, con]),
            svcsv ! {upgrade},
            ?MODULE:read();
        _ ->
            io:fwrite("{con} Got line '~s' from <stdin>~n", [Data]),
            netsv ! {send_privmsg, "#programming", Data},
            read()
    end.
