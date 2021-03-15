-module(auction_test).

-export([test_move_to_next_item/0, test_auction_complete/0, test_nothing_changed/0, stop/0, test_notification/0]).

test_move_to_next_item() ->
    auction_data:start_link(),
    pubsub:start_link(),
    {_,AuctionId} = auction_data:create_auction(),
    {_,[{ItemId, _}]} = auction_data:add_items(AuctionId,[{item1,desc1,100}]),
    auction_data:add_items(AuctionId,[{item2,desc2,100}]),
    io:format("Auction: ~p  Item: ~p \n", [AuctionId, ItemId]),
    auction:start_link(AuctionId), 
    Result = auction:bid(AuctionId, ItemId, 101, self()),
    io:format("Auction: ~p  Item: ~p Bid: ~p Result: ~p \n", [AuctionId, ItemId, 100, Result]).

test_auction_complete() ->
    auction_data:start_link(),
    pubsub:start_link(),
    {_,AuctionId} = auction_data:create_auction(),
    {_,[{ItemId, _}]} = auction_data:add_items(AuctionId,[{item1,desc1,100}]),
    io:format("Auction: ~p  Item: ~p \n", [AuctionId, ItemId]),
    auction:start_link(AuctionId), 
    Result = auction:bid(AuctionId, ItemId, 101, self()),
    io:format("Auction: ~p  Item: ~p Bid: ~p Result: ~p \n", [AuctionId, ItemId, 100, Result]).

test_nothing_changed() ->
    auction_data:start_link(),
    pubsub:start_link(),
    {_,AuctionId} = auction_data:create_auction(),
    {_,[{ItemId, _}]} = auction_data:add_items(AuctionId,[{item1,desc1,100}]),
    io:format("Auction: ~p  Item: ~p \n", [AuctionId, ItemId]),
    auction:start_link(AuctionId), 
    Result = auction:bid(AuctionId, ItemId, 101, self()),
    io:format("Auction: ~p  Item: ~p Bid: ~p Result: ~p \n", [AuctionId, ItemId, 100, Result]),
    auction:bid(AuctionId, ItemId, 105, self()).

test_notification() ->
    auction_data:start_link(),
    pubsub:start_link(),
    {_, Client1} = client:start_link(client1, 1000),
    {_, Client2} = client:start_link(client2, 2000),
    {_,AuctionId} = auction_data:create_auction(),
    {_,[{ItemId, _}]} = auction_data:add_items(AuctionId,[{item1,desc1,100}]),
    io:format("Auction: ~p  Item: ~p \n", [AuctionId, ItemId]),
    auction:start_link(AuctionId), 
    client:subscribe(Client1, AuctionId),
    client:subscribe(Client2, AuctionId),
    Result = auction:bid(AuctionId, ItemId, 101, self()),
    io:format("Auction: ~p  Item: ~p Bid: ~p Result: ~p \n", [AuctionId, ItemId, 100, Result]).

stop()->
    pubsub:stop(),
    auction_data:stop(),
    auction:stop().