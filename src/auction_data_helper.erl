-module(auction_data_helper).
-export([ping/1, add_items/2, get_items_id/2, to_response/1, create_auction/1]).

ping(_DbRef) -> {pong}.

create_auction(DbRef) ->
    ItemsTable = db:new(),
    db:add(ItemsTable, DbRef).

%add items to items table recursively
add_items(_DbRef, error) ->
    {error, unknown_auction};
add_items(_DbRef, []) ->
    [];
add_items(DbRef, [Element|Elements]) ->
    {ItemId, {Item, _, _}} = db:add(Element, DbRef),
    [{ItemId, Item}, add_items(DbRef, Elements)].

%get items table's id
get_items_id(_, []) -> error;
get_items_id(_, [{_,ID} | _ ]) -> ID;
get_items_id(DbRef, AuctionId) -> lists:last(db:read(AuctionId, DbRef)).

%convert response to error
to_response({error, Message})-> {error, Message};
to_response(Result) -> {ok, Result}.