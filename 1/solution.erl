-module(solution).
-export([cal_value_sum_from_file/1, cal_value/1, replace_words/1]).
-include_lib("eunit/include/eunit.hrl").

numbers() ->
    [
        {"one", "1"},
        {"two", "2"},
        {"three", "3"},
        {"four", "4"},
        {"five", "5"},
        {"six", "6"},
        {"seven", "7"},
        {"eight", "8"},
        {"nine", "9"}
    ].

% Replace numbers written as English words to integers.
replace_words(Line) ->
	replace_words(Line, numbers()).

replace_words(Line, []) ->
	lists:flatten(Line);

replace_words(Line, RemainingNumbers) ->
	[Pair|Tail] = RemainingNumbers,
	{SearchPattern, Replacement} = Pair,
	replace_words(
	  string:replace(Line, SearchPattern, Replacement, all), Tail
	 ).

% Calculate the calibration value by taking the first and the last digit
% and combining them into a base-10 integer.
cal_value(List) ->
	ReplacedList = replace_words(List),
	first_int(ReplacedList) * 10 + first_int(string:reverse(ReplacedList)).

% Returns the first digit found in the string.
first_int(List) ->
	[H|T] = List,
	case string:to_integer([H]) of
		{error, _} ->
			first_int(T);
		{Int, []} -> Int
	end.

% Given an IO device, it iterates over it line by line 
% and calculates the sum of calibration values.
cal_value_sum(Device, Acc) ->
	case io:get_line(Device, "") of
		eof -> Acc;
		Line -> cal_value_sum(Device, Acc+cal_value(Line))
	end.

cal_value_sum_from_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	cal_value_sum(Device, 0).

%%% Tests
cal_value_test_() ->
	[?_assert(cal_value("two1nine") =:= 29),
	 ?_assert(cal_value("eightwothree") =:= 83),
	 ?_assert(cal_value("abcone2threexyz") =:= 13),
	 ?_assert(cal_value("xtwone3four") =:= 24),
	 ?_assert(cal_value("4nineeightseven2") =:= 42),
	 ?_assert(cal_value("zoneight234") =:= 14),
	 ?_assert(cal_value("7pqrstsixteen") =:= 76)
	].
