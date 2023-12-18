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
	[CardL,Numbers] = string:split(Line, ": "),
	Card = list_to_integer(lists:last(string:split(CardL, "Card "))),
	[Winning, Ours] = lists:map(
			    fun(E) -> parse_numbers(E) end,
			    string:split(Numbers, " | ")
			   ),
	{Card, Winning, Ours}.

%%% Tests

parser_test_() ->
	Line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
	{CardNo, Winning, Ours} = solution:parse_line(Line),
	[?_assert(CardNo =:= 1),
	 ?_assert(Winning =:= [41,48,83,86,17]),
	 ?_assert(Ours =:= [83,86,6,31,17,9,48,53])
	].
