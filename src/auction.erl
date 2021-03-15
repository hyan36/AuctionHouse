-module(auction).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_cast/2, handle_call/3]).
-export([ping/0, stop/0, bid/4, check/2, timer/1, confirm/1, subscribe/1]).

ping() -> 
    gen_server:call({global, ?MODULE}, {ping, self()}).
stop() ->
    gen_server:cast({global, ?MODULE}, stop).

bid(AuctionId, ItemId, Bid, Bidder) -> 
    Request = {AuctionId,ItemId, Bid, Bidder},
    gen_server:call({global, ?MODULE}, {bid, self(), Request}).

timer({AuctionId, ItemId, Bid, Bidder}) -> 
    Request = {AuctionId,ItemId, Bid, Bidder},
    gen_server:cast({global, ?MODULE}, {timer, Request}).

confirm({AuctionId, ItemId, Bid, Bidder}) -> 
    Request = {AuctionId,ItemId, Bid, Bidder},
    io:format("Trying to confirm bid | Request: ~p  \n", [Request]),
    gen_server:cast({global, ?MODULE}, {confirm, Request}).

subscribe(AuctionId) -> pubsub:subscribe(AuctionId).

ping(_) -> {pong}.

start_link(AuctionId) ->
    Result = auction_data:get_items(AuctionId),
    pubsub:create_channel(AuctionId),
    case Result of
        {error, _} -> Result;
        {ok, []} -> {error, invalid_auction};
        {ok,[ItemId|_]} -> gen_server:start_link({global,?MODULE},?MODULE,{AuctionId, ItemId},[])
    end.

init({AuctionId, ItemId}) ->
    PastAuctions = db:new(),
    Sold = db:new(),
    {_, {_, _, Bid}} = auction_data:get_item(AuctionId, ItemId),
    io:format("Start auction: ~p, Item ~p \n", [AuctionId, ItemId]),
    pubsub:publish(AuctionId, {auction_event, auction_started}),
    State = {AuctionId, ItemId, Bid, self(), Sold, PastAuctions},
    auction_data:lock(),
    {ok, State}.

handle_call({ping, _Pid}, _From, State) -> %handle ping
    Reply = ping(State),
    {reply, Reply, State};
handle_call({bid, _Pid, Input}, _From, State) -> %handle bid
    case check(Input, State) of
        ok -> bid(Input, State);
        Error -> {reply, Error, State}
    end.

handle_cast(stop, State) ->
    {_, _, _, _, Sold, PastAuctions} = State,
    db:destroy(Sold),
    db:destroy(PastAuctions),
    {stop, normal,  State};
handle_cast({timer, Request}, State) ->
    io:format("Timer started ~p \n",[Request]), 
    timer:apply_after(10000, ?MODULE, confirm, [Request]),
    {noreply, State};
handle_cast({confirm, {AuctionId, ItemId, Bid, Bidder}}, {CurrentAuctionId, CurrentItemId, CurrentBid, CurrentBidder, Sold, PastAuctions}) 
    when (AuctionId == CurrentAuctionId) and (ItemId == CurrentItemId) and (Bid == CurrentBid) and (Bidder == CurrentBidder) ->
        io:format("Item ~p sold \n",[ItemId]),
        pubsub:publish(AuctionId, {auction_event, {item_sold, ItemId, Bid}}),
        Request =  {AuctionId, ItemId, Bid, Bidder},
        OldState = {CurrentAuctionId, CurrentItemId, CurrentBid, CurrentBidder, Sold, PastAuctions},
        db:write(ItemId, ItemId, Sold),
        auction_data:remove_item(AuctionId, ItemId),
        {_, Remaining} = auction_data:get_items(AuctionId),
        case Remaining of
            [] -> 
                db:write(AuctionId, AuctionId, PastAuctions),                
                io:format("All items sold, aunction completed: ~p | Request: ~p\n",[PastAuctions, Request]),
                pubsub:publish(AuctionId, {auction_event, auction_closed}),
                auction_data:unlock(), %Auction completed, can release the lock
                {noreply, {CurrentAuctionId, CurrentItemId, CurrentBid, CurrentBidder, Sold, PastAuctions}};
            [NextItemId|_] ->
                io:format("Move to new item: ~p | Request: ~p \n",[NextItemId, Request]),
                {_, {_, Desc, Bid}} = auction_data:get_item(AuctionId, NextItemId),
                pubsub:publish(AuctionId, {auction_event, {new_item, NextItemId, Desc, Bid }}),
                {noreply, {CurrentAuctionId, NextItemId, CurrentBid, CurrentBidder, Sold, PastAuctions}};
            _ ->
                io:format("Something went wrong. Request: ~p \n", [Request]), 
                {noreply, OldState}
        end;
handle_cast({confirm, _}, State) -> 
        io:format("Nothing changed \n"), 
        {noreply, State}.

check({AuctionId, _, _, _}, {CAId,_, _, _, _, _}) 
    when AuctionId /= CAId  -> {error, invalid_auction};
check({_, ItemId, _, _}, {_, CurrentItemId, _, _, _, _}) 
    when ItemId /= CurrentItemId  -> {error, invalid_item};
check({AuctionId, ItemId, _, _}, {CAId, CurrentItemId, _, _,  Sold, PastAuctions}) 
    when (AuctionId == CAId) and (ItemId == CurrentItemId) -> 
    case db:read(AuctionId, PastAuctions) of
        [] -> 
        case db:read(ItemId, Sold) of
            [] -> ok;
            [_] -> {error, item_sold}
        end;
        [_] -> {error, auction_ended}
    end.

bid({_, _, Bid, Bidder}, {AuctionId, ItemId, CurrentBid, _, Sold, PastAuctions}) 
    when Bid > CurrentBid -> 
        io:format("Bid successfuly ~p | Bid: ~p  | Bidder: ~p \n",[ItemId, Bid, Bidder]),
        pubsub:publish(AuctionId, {auction_event, {new_bid, ItemId, Bid, Bidder}}),
        State = {AuctionId, ItemId, Bid, Bidder, Sold, PastAuctions},
        timer({AuctionId, ItemId, Bid, Bidder}),
        {reply, {ok, leading}, State};
bid(Request , State) -> 
    {_, ItemId, Bid, Bidder} = Request,
    io:format("Bid failed ~p | Bid: ~p  | Bidder: ~p \n",[ItemId, Bid, Bidder]),
    {_,_,CurrentBid, _, _, _} = State,
    {reply, {ok, {not_leading, CurrentBid}}, State}.
