-module(hash_cloud).
-author("armanddp@identitude.com").

-export([search/1, spawn_search/1]).

search(Keywords) ->
	%% TODO: Figure out where to configure this from
	application:start(inets),
	twitter_stream:search(Keywords, 2, 1).

spawn_search(Keywords) ->
	spawn(hash_cloud, search, [Keywords]).
	