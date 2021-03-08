-module(auction_data).
-export([ping/0, start_link/0, init/1, ping/1, handle_call/3, handle_cast/2, stop/0, create_auction/0, add_items/2]).

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

add_items(AuctionId, Items) ->
    gen_server:call(?MODULE, {add_items, self(), AuctionId, Items}).

ping(_DbRef) -> {pong}.

%add items to items table recursively
add_items(DbRef, AuctionId, Elements) -> 
    ItemsTable = auction_data_helper:get_items_id(DbRef, AuctionId),
    auction_data_helper:add_items(ItemsTable, Elements).

handle_call({ping, _Pid}, _From, Data) -> %handle ping
    Reply = auction_data_helper:ping(Data),
    {reply, Reply, Data};
handle_call({create_auction, _Pid}, _From, Data) -> %handle create auction event
    {Key, _} = auction_data_helper:create_auction(Data),
    {reply, {ok, Key}, Data};
handle_call({add_items, _Pid, AuctionId, Elements}, _From, Data) -> %handle add elements
    Result = add_items(Data, AuctionId, Elements),
    Response = auction_data_helper:to_response(Result),
    {reply, Response, Data}.
handle_cast(stop, Data) ->
    db:destroy(Data),
    {stop, normal, Data}.
