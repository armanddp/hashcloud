-module(reverse).
-export([reverse/1]).

reverse(List) -> 
	reverse(List, []).
	
reverse([Head | Rest], Reversed_List) -> 
	io:format("Head ~w | Rest ~w Reversed List ~w ~n", [Head, Rest, Reversed_List]),
	reverse(Rest, [Head | Reversed_List]);

reverse([], Reversed_List) -> 
	Reversed_List.