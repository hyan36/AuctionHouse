-module(auction_data_helper).
-export([ping/1, add_items/3, add_items/2, get_items_id/2, to_response/1, create_auction/1, get_auctions/1, get_items/1, get_items/2, get_item/2, get_item/3, get_item_result/1, remove_auction/2, remove_auction/3, remove_item/2, remove_item/3, remove_item_result/3]).

ping(_DbRef) -> {pong}.

create_auction(DbRef) ->
    ItemsTable = db:new(),
    db:add(ItemsTable, DbRef).

get_auctions(DbRef) -> db:keys(DbRef).

%get items
get_items(error) -> {error, unknown_auction};
get_items(ItemsID) -> db:keys(ItemsID).

%get items
get_items(DbRef, AuctionId) ->
    ItemsTable = get_items_id(DbRef, AuctionId),
    get_items(ItemsTable).

%get item
get_item(_ItemId, error) ->
    {error, unknown_auction};
get_item(ItemId, ItemsTable) ->
    get_item_result(db:read(ItemId, ItemsTable)).
get_item(DbRef, AuctionId, ItemId) ->
    ItemsTable = get_items_id(DbRef, AuctionId),
    get_item(ItemId, ItemsTable).
get_item_result([]) -> {error, unknown_item};
get_item_result(Result) -> [{_, Value} | _ ]= Result, Value.

%add items to items table recursively
add_items(_DbRef, error) ->
    {error, unknown_auction};
add_items(_DbRef, []) ->
    [];
add_items(DbRef, [Element|Elements]) ->
    {ItemId, {Item, _, _}} = db:add(Element, DbRef),
    [{ItemId, Item}|add_items(DbRef, Elements)].

%add items to items table recursively%add items to items table recursively
add_items(DbRef, AuctionId, Elements) -> 
    ItemsTable = get_items_id(DbRef, AuctionId),
    add_items(ItemsTable, Elements).

%get items table's id
get_items_id([]) -> error;
get_items_id([ Element | _Elements ]) -> 
    {_, ItemsID} = Element,
    ItemsID.
get_items_id(DbRef, AuctionId) -> get_items_id(db:read(AuctionId, DbRef)).

%remove auctions
remove_auction(_DbRef, _AuctionId, []) -> {error, unknown_auction};
remove_auction(DbRef, AuctionId, _) -> db:delete(AuctionId, DbRef).
remove_auction(DbRef, AuctionId) -> 
    Auctions = db:read(AuctionId, DbRef),
    remove_auction(DbRef, AuctionId, Auctions).

remove_item(_, error) -> {error, unknown_auction};
remove_item(ItemId, ItemsTable) -> 
    Item = db:read(ItemId, ItemsTable),
    remove_item_result(Item, ItemId, ItemsTable).
   
remove_item(DbRef, AuctionId, ItemId) ->
    ItemsTable = get_items_id(DbRef, AuctionId),
    remove_item(ItemId, ItemsTable).

remove_item_result([], _, _) -> {error, unknown_item};
remove_item_result(_, ItemId, ItemsTable) -> db:delete(ItemId, ItemsTable).

%convert response to error
to_response({error, Message})-> {error, Message};
to_response(Result) -> {ok, Result}.