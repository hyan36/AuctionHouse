-module(auction_data).
-export([ping/0, start_link/0, init/1, ping/1, handle_call/3, handle_cast/2, stop/0, create_auction/0]).

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

ping(_DbRef) -> {pong}.

create_auction(DbRef) ->
    ItemsTable = db:new(),
    db:add(ItemsTable, DbRef).

%add items to items table recursively
add_items(_DbRef, error) ->
    error;
add_items(_DbRef, []) ->
    [];
add_items(DbRef, [Element|Elements]) ->
    {ItemId, {Item, _, _}} = db:add(Element, DbRef),
    [{ItemId, Item}, add_items(DbRef, Elements)].
add_items(DbRef, AuctionId, Elements) -> 
    ItemsTable = get_items_id(DbRef, AuctionId),
    add_items(ItemsTable, Elements).


handle_call({ping, _Pid}, _From, Data) -> %handle ping
    Reply = ping(Data),
    {reply, Reply, Data};
handle_call({create_auction, _Pid}, _From, Data) -> %handle create auction event
    {Key, _} = create_auction(Data),
    {reply, {ok, Key}, Data};
handle_call({add_items, _Pid, AuctionId, Elements}, _From, Data) -> %handle add elements
    Result = add_items(Data, AuctionId, Elements),
    Response = to_response(Result),
    {reply, Response, Data}.
handle_cast(stop, Data) ->
    db:destroy(Data),
    {stop, normal, Data}.

get_items_id(_, []) -> error;
get_items_id(_, [{_,ID} | _ ]) -> ID;
get_items_id(DbRef, AuctionId) -> lists:last(db:read(AuctionId, DbRef)).

to_response(error)-> {error, unknown_auction};
to_response(Result) -> {ok, Result}.
