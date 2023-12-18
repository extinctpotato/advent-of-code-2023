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

scratchcard_value(0) -> 0;
scratchcard_value(Length) -> math:pow(2, Length-1).

line_value(Line) ->
	{_, Winning, Ours} = parse_line(Line),
	scratchcard_value(length([X || X <- Winning, Y <- Ours, X =:= Y])).

%%% Tests

parser_test_() ->
	Line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
	{CardNo, Winning, Ours} = solution:parse_line(Line),
	[?_assert(CardNo =:= 1),
	 ?_assert(Winning =:= [41,48,83,86,17]),
	 ?_assert(Ours =:= [83,86,6,31,17,9,48,53])
	].

line_value_test_() ->
	Cases = [{"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", 8},
		 {"Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", 2},
		 {"Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 2},
		 {"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", 1},
		 {"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", 0},
		 {"Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", 0}
		],
	[?_assert(line_value(L) == V) || {L,V} <- Cases].
