-module(client_test).
-export([test/0]).

test() 
    -> 
    auction_data:start_link(),
    pubsub:start_link(),
    {_, Client1} = client:start_link(client1, 1000),
    {_, Client2} = client:start_link(client2, 5000),
    {_,AuctionId} = auction_data:create_auction(),
    {_,[{ItemId, _}]} = auction_data:add_items(AuctionId,[{item1,desc1,100}]),
    auction:start_link(AuctionId), 
    client:subscribe(Client1, AuctionId),
    client:subscribe(Client2, AuctionId),
    client:bid(Client1, AuctionId, ItemId, 101),
    io:format("Test ~p \n",[Client2]),
    client:bid(Client2, AuctionId, ItemId, 101),
    
    ok.