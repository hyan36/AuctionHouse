-module(client).
-behaviour(gen_server).



ping(Name, _DbRef) -> {Name, pong}.


start_link(Name) ->
    gen_server:start_link({global,Name}, Name,[],[]).


handle_call({ping, _Pid, Name}, _From, State) -> %handle ping
    Reply = ping(Name,State),
    {reply, Reply, State}.
