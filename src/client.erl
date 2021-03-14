-module(client).
-behaviour(gen_server).
-export([start_link/1, handle_call/3,  handle_cast/2, init/1]).
-export([stop/1, ping/1, notify/2, subscribe/2]).


ping(BidderId) -> 
    gen_server:call(BidderId, {ping}).
stop(BidderId) ->
    gen_server:cast(BidderId, stop).
subscribe(BidderId, Channel)->
    gen_server:call(BidderId, {subscribe, Channel}).
notify(BidderId, Message) ->
    gen_server:cast(BidderId, {notify, Message}).

ping() -> {pong}.

init(Arg) ->
    {ok, Arg}.

start_link(Bidder) ->
    gen_server:start_link({global, Bidder}, ?MODULE , Bidder, []).

handle_call({ping}, _From, State) -> %handle ping
    Reply = ping(),
    {reply, Reply, State};
handle_call({subscribe, Channel}, _From, State) -> %handle ping
    Reply = pubsub:subscribe(Channel),
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({notify, Message}, State) ->
    io:format("Message Recieved @ ~p : ~p \n",[self(), Message]),
    {noreply, State}.
