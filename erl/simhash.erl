-module(simhash).
 
-define(HASH_RANGE, 1 bsl 128).
-define(HASH_ACCU, 12).
-define(HASH_WIDTH, 128).
-export([hash_file/1]).
 
hash_file(File) ->
  {ok, Binary} = file:read_file(File),
  Tokens = re:split(Binary, "\\W"),
  calculate_simhash(Tokens).
 
calculate_hash(A) ->
  %% Hash = erlang:phash2(A, ?HASH_RANGE),
  << Hash:(?HASH_WIDTH) >> = erlang:md5(A),
  Hash.
 
calculate_simhash(Tokens) ->
  FeatureHashes = [calculate_hash(A) || A <- Tokens, A =/= <<>>],
  {HashAcc, Len} = lists:foldl(fun accumulate_simhash/2, {0,0} , FeatureHashes),
  << <<(is_hash_valid(B, (Len / 2))):1>> || 
     <<B:(?HASH_ACCU)>> <= <<HashAcc:(?HASH_WIDTH * ?HASH_ACCU)>> >>.
 
accumulate_simhash(Hash, {Accum, L}) ->
	io:format("~w ~n", [?HASH_WIDTH * ?HASH_ACCU]),
  <<A:(?HASH_WIDTH * ?HASH_ACCU)>> = 
          << <<B:(?HASH_ACCU)>> || <<B:1>> <= << Hash:(?HASH_WIDTH) >> >>,
  {Accum + A, L + 1}.
 
is_hash_valid(E, Len) ->
  case(E > Len) of 
    true -> 1; 
    _ -> 0 
  end.