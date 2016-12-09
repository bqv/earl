-compile({nowarn_unused_function, [mk_conf/4, mk_conf/5]}).
-record(conf, {host, port, chan, wMin, wCur, node}).

mk_conf(Host, Port, Chan, WNum) ->
    #conf{  host = Host,
            port = Port,
            chan = Chan,
            wMin = WNum,
            wCur = WNum
            }.

mk_conf(Host, Port, Chan, WNum, Node) ->
    #conf{  host = Host,
            port = Port,
            chan = Chan,
            wMin = WNum,
            wCur = WNum,
            node = Node
            }.
