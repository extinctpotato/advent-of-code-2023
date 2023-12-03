-module(solution).
-export([cal_value_sum_from_file/1, cal_value/1, replace_words/1]).

numbers() ->
    [
        {"one", 1},
        {"two", 2},
        {"three", 3},
        {"four", 4},
        {"five", 5},
        {"six", 6},
        {"seven", 7},
        {"eight", 8},
        {"nine", 9}
    ].

replace_words(Line) ->
	replace_words(Line, numbers()).

replace_words(Line, []) ->
	lists:flatten(Line);

replace_words(Line, RemainingNumbers) ->
	[Pair|Tail] = RemainingNumbers,
	{SearchPattern, Replacement} = Pair,
	replace_words(
	  string:replace(Line, SearchPattern, integer_to_list(Replacement), all), Tail
	 ).

cal_value(List) ->
	first_int(List) * 10 + first_int(string:reverse(List)).

first_int(List) ->
	[H|T] = List,
	case string:to_integer([H]) of
		{error, _} ->
			first_int(T);
		{Int, []} -> Int
	end.

cal_value_sum(Device, Acc) ->
	case io:get_line(Device, "") of
		eof -> Acc;
		Line -> cal_value_sum(Device, Acc+cal_value(replace_words(Line)))
	end.

cal_value_sum_from_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	cal_value_sum(Device, 0).
