-module(db).
-export([new/0, destroy/1, add/2, delete/2, read/2, match/2,  keys/1, elements/1]).

new() -> ets:new(?MODULE, []).

destroy(DbRef) -> ets:delete(DbRef), ok.

add(Element, DbRef) -> 
    Key =  make_ref(),
    ets:insert(DbRef, {Key, Element}),
    {Key, Element}.

delete(Key, DbRef) -> ets:delete(DbRef, Key).

read(Key, DbRef) -> ets:lookup(DbRef, Key).

match(Value, DbRef) -> ets:match(DbRef, {'$1', Value}).

keys(_DbRef, '$end_of_table') -> [];
keys(DbRef, N) -> 
    Key = ets:next(DbRef, N),
    add_keys(Key, keys(DbRef, Key)).

keys(DbRef) ->
    Key = ets:first(DbRef),
    add_keys(Key, keys(DbRef, Key)).

add_keys('$end_of_table', Keys) -> Keys;
add_keys(Key, Keys) -> [Key | Keys].

elements(_DbRef, []) -> [];
elements(DbRef, [Key|Keys]) -> 
    Value = ets:lookup(DbRef, Key),
    [{Key, Value}, elements(DbRef, Keys)].

elements(DbRef) -> 
    Keys = keys(DbRef),
    elements(DbRef, Keys).




