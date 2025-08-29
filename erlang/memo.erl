%% memo.erl
%% Memoize a function that accepts a list of primitive args.
%% Primitives: integers, atoms (true/false), binaries, etc.
%% We use nested maps as a trie keyed directly by those terms.

-module(memo).
-export([memoize/1]).

-define(LEAF, '$leaf').

%% Public API:
%% Fun must accept a single argument: a list of args (e.g., [2,3,5]).
%% Returns a fun that takes that same list and caches by the list as a path.
memoize(Fun) when is_function(Fun, 1) ->
  ServerPid = spawn(fun() -> memo_server_loop(Fun, #{}) end),
  fun(ArgsList) when is_list(ArgsList) ->
      call_server(ServerPid, ArgsList)
  end.

%% Client: sync request/reply to the memo server
call_server(Pid, Args) ->
  Ref = make_ref(),
  Pid ! {lookup, self(), Ref, Args},
  receive
    {Ref, Value} -> Value
  end.

%% Memo server loop
memo_server_loop(Fun, Trie) ->
  receive
    {lookup, From, Ref, Args} ->
      case trie_lookup(Args, Trie) of
        {hit, Value} ->
          From ! {Ref, Value},
          memo_server_loop(Fun, Trie);
        miss ->
          Value = Fun(Args),
          NewTrie = trie_insert(Args, Value, Trie),
          From ! {Ref, Value},
          memo_server_loop(Fun, NewTrie)
      end
  end.

%% Trie operations (nested maps by arg path)
trie_lookup([], Trie) ->
  case maps:get(?LEAF, Trie, undefined) of
    undefined -> miss;
    V -> {hit, V}
  end;
trie_lookup([K | Rest], Trie) ->
  case maps:get(K, Trie, undefined) of
    undefined -> miss;
    SubTrie when is_map(SubTrie) -> trie_lookup(Rest, SubTrie);
    _ -> miss
  end.

trie_insert([], Value, Trie) ->
  Trie#{?LEAF => Value};
trie_insert([K | Rest], Value, Trie) ->
  Sub0 = maps:get(K, Trie, #{}),
  Sub1 = trie_insert(Rest, Value, Sub0),
  Trie#{K => Sub1}.
