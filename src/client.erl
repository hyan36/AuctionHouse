-module(client).
-behaviour(gen_server).
-export([start_link/2, handle_call/3,  handle_cast/2, init/1]).
-export([stop/1, ping/1, notify/2, subscribe/2, bid/4]).


ping(BidderId) -> 
    gen_server:call(BidderId, {ping}).
bid(BidderId, AuctionId, ItemId, Bid) -> 
    gen_server:call(BidderId, {bid, AuctionId, ItemId, Bid}).
stop(BidderId) ->
    gen_server:cast(BidderId, stop).
subscribe(BidderId, Channel)->
    gen_server:call(BidderId, {subscribe, Channel}).
notify(BidderId, Message) ->
    gen_server:cast(BidderId, {notify, Message}).


ping() -> {pong}.

start_link(Bidder, Budget) ->
    gen_server:start_link({global, Bidder}, ?MODULE , {Bidder, Budget}, []).

init({BidderName, Budget}) ->
    LastBid = {auction_id, item_id, 0},
    {ok, {BidderName, Budget, LastBid}}.
    

handle_call({ping}, _From, State) -> %handle ping
    Reply = ping(),
    {reply, Reply, State};
handle_call({subscribe, Channel}, _From, State) -> %handle ping
    Reply = pubsub:subscribe(Channel),
    {reply, Reply, State};
handle_call({bid, AuctionId, ItemId, Bid}, _From, State) ->
    Reply = auction:bid(AuctionId, ItemId, Bid, self()),
    {BidderName, Budget, _} = State,
    NewState = {BidderName, Budget, {AuctionId, ItemId, Bid}},
    case Reply of
        {error, _} -> 
            {reply, Reply,  State};
        {ok, leading}  -> 
            {reply, Reply, NewState};
        {ok, {not_leading, CurrentBid}} -> 
            NewBid = CurrentBid * 1.2,
            auto_bid(NewBid, Budget, AuctionId, ItemId),            
            {reply, Reply, NewState}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({notify, {auction_event, {new_bid, ItemId, Bid, Bidder }}}, State) ->
    io:format("Item ~p has a new bid ~p \n",[ItemId, Bid]),
    {BidderName, Budget, {CurrentAuctionId, CurrentItemId, CurrentBid}} = State,
    case ((ItemId == CurrentItemId) and (CurrentBid =< Bid) and (Bidder /= self())) of
        true -> 
           io:format("Someone out bid for ~p with by: ~p \n",[ItemId, Bid]),
           NewBid = Bid * 1.2,
           auto_bid(NewBid, Budget, CurrentAuctionId, CurrentItemId), 
           {noreply,  {BidderName, Budget, {CurrentAuctionId, CurrentItemId, NewBid}}};
        false ->
            {noreply, State}
   end;
handle_cast({notify, Message}, State) ->
    io:format("Message Recieved @ ~p : ~p \n",[self(), Message]),
    {noreply, State};
handle_cast({auto_bid, AuctionId, ItemId, NewBid}, State) ->
    auction:bid(AuctionId, ItemId, NewBid, self()),
    {noreply, State}.


auto_bid(NewBid, Budget, AuctionId, ItemId) 
    when NewBid < Budget -> 
        io:format("Increasing Bid \n"),
        gen_server:cast(self(), {auto_bid, AuctionId, ItemId, NewBid});
auto_bid(_, _, _, _) -> 
    io:format("Exceeding budget, give up \n"),
    {ok, give_up}. 