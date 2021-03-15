-module(pubsub_test).

-export([test/0]).

test() -> 
    {_, Client1} = client:start_link(client1, 1000),
    {_, Client2} = client:start_link(client2, 2000),
    pubsub:start_link(),
    pubsub:create_channel(123),
    client:subscribe(Client1, 123),
    client:subscribe(Client2, 123),    
    pubsub:publish(123, {ok, test_message}),
    client:stop(Client1),
    client:stop(Client2),
    pubsub:stop().