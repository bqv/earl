-compile({nowarn_unused_function, [mk_conf/3]}).
-record(conf, {host, port, wMin}).

mk_conf(Host, Port, WMin) ->
    #conf{  host = Host,
            port = Port,
            wMin = WMin
            }.
