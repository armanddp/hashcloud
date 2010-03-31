-module(bootstrap).

-export([startup/0, create_tables/0]).
-include("tweet.hrl").

startup() ->
	application:start(mnesia).

create_tables() ->
	error_logger:info_msg("Creating Tables"),
	startup(),
	mnesia:delete_table(tweet),
	mnesia:create_table(tweet, 
	    [{attributes, record_info(fields, tweet)}]).


