-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

whitespace_split(String) ->
	{ok, R} = re:compile("\\s+"),
	re:split(String, R, [{return,list}]).

parse_numbers(String) ->
	lists:map(
	  fun(E) -> element(1, string:to_integer(E)) end,
	  whitespace_split(String)
	 ).

parse_line(Line) ->
	[CardL|Numbers] = Line,
	Card = list_to_integer(lists:last(string:split(CardL, "Card "))),
	{Card}.
