-module(pubsub).
-behaviour(gen_server).
-export([start_link/0, handle_call/3,  handle_cast/2, init/1]).
-export([stop/0, create_channel/1, delete_channel/1, unsubscribe/1, subscribe/1, publish/2]).

stop() -> gen_server:cast({global, ?MODULE}, stop).

create_channel(Channel) 
    -> gen_server:call({global,?MODULE},{create_channel, Channel}).

delete_channel(Channel) 
    -> gen_server:call({global,?MODULE},{delete_channel, Channel}).

subscribe(Channel)
    -> gen_server:call({global,?MODULE},{subscribe, self(), Channel}).

unsubscribe(Channel)
    -> gen_server:call({global,?MODULE},{unsubscribe, self(), Channel}).

publish(Channel, Message)
    -> gen_server:call({global,?MODULE},{publish, self(), Channel, Message}).

start_link()->
    gen_server:start_link({global, ?MODULE}, ?MODULE ,[],[]).

init(_Arg) ->
    Channels = db:new(),
    {ok, Channels}.

handle_call({create_channel, Channel}, _From, State) ->
    Result = 
    case db:read(Channel, State) of 
        [] ->  
            io:format("Creating channel: ~p \n", [Channel]),
            db:write(Channel,  db:new(), State),
            ok;
        [_] ->
            io:format("Channel already exsit \n"), 
            {error, duplicate_channel}
    end,
    {reply, Result, State};
handle_call({delete_channel, Channel}, _From, State) ->
    Result = 
    case db:read(Channel, State) of 
        [] ->
            io:format("Can't delete unknown channel \n"),  
            {error, unknown_channel};
        [_] -> 
            io:format("Deleteting channel: ~p \n", [Channel]),
            db:delete(Channel, State),
            ok
    end,
    {reply, Result, State};
handle_call({subscribe, Pid, Channel}, _From, State) ->
    Subscribers = db:read(Channel, State),
    Result = 
    case Subscribers of 
        [] ->
            io:format("Can't subscribe unknown channel \n"),  
            {error, unknown_channel};
        [{_, STable}|_] -> 
            io:format("Pid ~p subscribe to channel: ~p \n", [Pid, Channel]),
            db:write(Pid, Pid, STable),
            ok
    end,
    {reply, Result, State};
handle_call({unsubscribe, Pid, Channel}, _From, State) ->
    Subscribers = db:read(Channel, State),
    Result = 
    case Subscribers of 
        [] ->
            io:format("Can't subscribe unknown channel \n"),  
            {error, unknown_channel};
        [{_, STable}|_] -> 
            io:format("Pid ~p unsubscribe subscribe to channel: ~p \n", [Pid, Channel]),
            db:delete(Pid, STable),
            ok
    end,
    {reply, Result, State};
handle_call({publish, _Pid, Channel, Event}, _From, State) ->
    Subscribers = db:read(Channel, State),
    Result = 
    case Subscribers of 
        [] ->
            io:format("Can't subscribe unknown channel \n"),  
            {error, unknown_channel};
        [{_, STable}|_] -> 
            Clients = db:keys(STable),
            io:format("Pushing ~p to ~p \n", [Event, Clients]),
            push(Clients, Event),
            ok
    end,
    {reply, Result, State}.
    
push([], _) -> ok;
push([Client| Clients ], Event) ->
    client:notify(Client, Event),
    push(Clients, Event).

handle_cast(stop, State) ->
    db:destroy(State),
    {stop, normal, State}.