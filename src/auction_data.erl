-module(auction_data).
-export([ping/0, start_link/0, init/1, ping/1, handle_call/3, handle_cast/2, stop/0, create_auction/0, add_items/2, get_auctions/0, get_items/1, get_item/2]).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) -> 
    Data = db:new(),
    {ok, Data}.
ping() -> 
    gen_server:call(?MODULE, {ping, self()}).
stop() ->
    gen_server:cast(?MODULE, stop).

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

ping(_DbRef) -> {pong}.

handle_call({ping, _Pid}, _From, Data) -> %handle ping
    Reply = auction_data_helper:ping(Data),
    {reply, Reply, Data};
handle_call({create_auction, _Pid}, _From, Data) -> %handle create auction event
    {Key, _} = auction_data_helper:create_auction(Data),
    {reply, {ok, Key}, Data};
handle_call({get_auctions, _Pid}, _From, Data) -> %handle create auction event
    Result = auction_data_helper:get_auctions(Data),
    {reply, {ok, Result}, Data};
handle_call({add_items, _Pid, AuctionId, Elements}, _From, Data) -> %handle add elements
    Result = auction_data_helper:add_items(Data, AuctionId, Elements),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, Data};
handle_call({get_items, _Pid, AuctionId}, _From, Data) -> %handle add elements
    Result = auction_data_helper:get_items(Data, AuctionId),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, Data};
handle_call({get_item, _Pid, AuctionId, ItemId}, _From, Data) -> %get an item from auction
    Result = auction_data_helper:get_item(Data, AuctionId, ItemId),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, Data}.
handle_cast(stop, Data) ->
    db:destroy(Data),
    {stop, normal, Data}.
