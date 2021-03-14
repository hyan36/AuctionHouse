-module(auction_data).
-behaviour(gen_server).
-export([ping/0, start_link/0, init/1, ping/1, handle_call/3, handle_cast/2, stop/0]).
-export([create_auction/0, add_items/2, get_auctions/0, get_items/1, get_item/2, remove_auction/1, remove_item/2,lock/0, unlock/0]).

ping() -> 
    gen_server:call(?MODULE, {ping, self()}).
stop() ->
    gen_server:cast(?MODULE, stop).
lock() ->
    gen_server:cast(?MODULE, lock).
unlock() ->
    gen_server:cast(?MODULE, unlock).

create_auction() -> 
    gen_server:call(?MODULE, {create_auction, self()}).

get_auctions() ->
    gen_server:call(?MODULE, {get_auctions, self()}).

add_items(AuctionId, Items) ->
    gen_server:call(?MODULE, {add_items, self(), AuctionId, Items}).

get_items(AuctionId) ->
    gen_server:call(?MODULE, {get_items, self(), AuctionId}).

get_item(AuctionId, ItemId) ->
    gen_server:call(?MODULE, {get_item, self(), AuctionId, ItemId}).

remove_auction(AuctionId) ->
    gen_server:call(?MODULE, {remove_auction, self(), AuctionId}).

remove_item(AuctionId, ItemId) ->
    gen_server:call(?MODULE, {remove_item, self(), AuctionId, ItemId}).

ping(_DbRef) -> {pong}.


start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) -> 
    Data = db:new(),
    Lock = false,
    {ok, {Data, Lock}}.

handle_call({ping, _Pid}, _From, Data) -> %handle ping
    Reply = auction_data_helper:ping(Data),
    {reply, Reply, Data};
handle_call({create_auction, _Pid}, _From, State) -> %handle create auction event
    {Data, _} = State,
    {Key, _} = auction_data_helper:create_auction(Data),
    {reply, {ok, Key}, State};
handle_call({get_auctions, _Pid}, _From, State) -> %handle create auction event
    {Data, _} = State,
    Result = auction_data_helper:get_auctions(Data),
    {reply, {ok, Result}, State};
handle_call({add_items, _Pid, AuctionId, Elements}, _From, {Data, Lock}) 
    when Lock == false -> %handle add elements when server is not locked
        Result = auction_data_helper:add_items(Data, AuctionId, Elements),
        Response = auction_data_helper:to_response(Result),
        {reply, Response,  {Data, Lock}};
handle_call({add_items, _Pid, _AuctionId, _Elements}, _From, State)  
    -> %user can not add any items when item is lockded
        {reply, {error, db_locked}, State};
handle_call({get_items, _Pid, AuctionId}, _From, State) -> %handle add elements
    {Data, _} = State,
    Result = auction_data_helper:get_items(Data, AuctionId),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, State};
handle_call({get_item, _Pid, AuctionId, ItemId}, _From, State) -> %get an item from auction
    {Data, _} = State,
    Result = auction_data_helper:get_item(Data, AuctionId, ItemId),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, State};
handle_call({remove_auction, _Pid, AuctionId}, _From, State) -> %get an item from auction
    {Data, _} = State,
    Result = auction_data_helper:remove_auction(Data, AuctionId),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, State};
handle_call({remove_item, _Pid, AuctionId, ItemId}, _From, State) -> %get an item from auction
    {Data, _} = State,
    Result = auction_data_helper:remove_item(Data, AuctionId, ItemId),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, State}.
    
handle_cast(stop, Data) ->
    db:destroy(Data),
    {stop, normal, Data};
handle_cast(lock, {Data, _}) ->
    io:format("locking data server \n"),
    {noreply, {Data, true}};
handle_cast(unlock, {Data, _}) ->
    io:format("unlocking data server \n"),
    {noreply, {Data, false}}.
