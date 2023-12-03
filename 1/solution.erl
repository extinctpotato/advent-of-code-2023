-module(solution).
-export([cal_value_sum_from_file/1]).

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
		Line -> cal_value_sum(Device, Acc+cal_value(Line))
	end.

cal_value_sum_from_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	cal_value_sum(Device, 0).
