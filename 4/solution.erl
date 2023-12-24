-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

whitespace_split(String) ->
	{ok, R} = re:compile("\\s+"),
	[X || X <- re:split(String, R, [{return,list}]), length(X) > 0].

parse_numbers(String) ->
	lists:map(
	  fun(E) -> element(1, string:to_integer(E)) end,
	  whitespace_split(String)
	 ).

parse_line(Line) ->
	[CardL,Numbers] = string:split(Line, ": "),
	Card = list_to_integer(string:trim(lists:last(string:split(CardL, "Card ")))),
	[Winning, Ours] = lists:map(
			    fun(E) -> parse_numbers(E) end,
			    string:split(Numbers, " | ")
			   ),
	{Card, Winning, Ours}.

scratchcard_value(0) -> 0;
scratchcard_value(Length) -> math:pow(2, Length-1).

matching_numbers(List1, List2) ->
	length([X || X <- List1, Y <- List2, X =:= Y]).

line_value(Line) ->
	{_, Winning, Ours} = parse_line(Line),
	scratchcard_value(matching_numbers(Winning, Ours)).

won_copies(Line) ->
	{Number, Winning, Ours} = parse_line(Line),
	case matching_numbers(Winning, Ours) of
		0 -> [];
		Matching -> lists:seq(Number+1, Number+Matching)
	end.

won_scratchcards(_LeftElement, [], _Originals, Acc) ->
	Acc;
won_scratchcards(LeftElement, RightElements, Originals, Acc) ->
	case lists:nth(LeftElement, Originals) of % these are new won cards
		[] -> % go back to the non-LHS of the tree
			[NewLeft|NewRight] = RightElements,
			won_scratchcards(NewLeft, NewRight, Originals, Acc);
		List ->
			[NewLeft|PrependRight] = List,
			won_scratchcards(
			  NewLeft,
			  lists:append(PrependRight, RightElements),
			  Originals,
			  Acc+1
			 )
	end.

won_scratchcards(Card, Originals) ->
	won_scratchcards(Card, lists:nth(Card, Originals), Originals, 1).

%%% Tests

sample_line_cases() ->
	[{"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", 8, [2,3,4,5]},
	 {"Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", 2, [3,4]},
	 {"Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 2, [4,5]},
	 {"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", 1, [5]},
	 {"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", 0, []},
	 {"Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", 0, []}
	].

sample_original_cards() ->
	original_cards([L || {L,_,_} <- sample_line_cases()], lines).

parser_test_() ->
	Line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
	{CardNo, Winning, Ours} = solution:parse_line(Line),
	[?_assert(CardNo =:= 1),
	 ?_assert(Winning =:= [41,48,83,86,17]),
	 ?_assert(Ours =:= [83,86,6,31,17,9,48,53])
	].

line_value_test_() ->
	[?_assert(line_value(L) == V) || {L,V,_} <- sample_line_cases()].

process_file_test_() ->
	[?_assert(process_file("input2.txt") == 13)].

won_copies_test_() ->
	[?_assert(won_copies(L) =:= V) || {L,_,V} <- sample_line_cases()].

%%% File processing

original_cards(Device, device, Cards) ->
	case io:get_line(Device, "") of
		eof -> lists:reverse(Cards);
		Line -> original_cards(Device, device, [won_copies(string:chomp(Line))|Cards])
	end.

original_cards(Lines, lines) ->
	lists:map(fun(L) -> won_copies(L) end, Lines);
original_cards(Device, device) ->
	original_cards(Device, device, []).

process_lines2(Device) ->
	Originals = original_cards(Device, device),
	lists:sum(
	  lists:map(
	    fun(C) -> won_scratchcards(C, Originals) end,
	    lists:seq(1, length(Originals))
	   )
	 ).

process_lines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof -> Acc;
		L ->
			Value = line_value(string:chomp(L)),
			process_lines(Device, Acc + Value)
	end.

process_file2(Path) ->
	{_, Device} = file:open(Path, [read]),
	process_lines2(Device).

process_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	process_lines(Device, 0).
