-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

number_indices(Line, [], [], _Index, true) ->
	{Line, 0, []};
number_indices(Line, Digits, Indices, _Index, true) ->
	{Int, []} = string:list_to_integer(Digits),
	{Line, Int, Indices};
number_indices([], Digits, Indices, Index, _Halt) ->
	number_indices([], Digits, Indices, Index, true);
number_indices(Line, Digits, Indices, Index, _Halt) ->
	[H|T] = Line,
	Index2 = Index + 1,
	case string:to_integer([H]) of
		{error, _} ->
			{Halt, Line2} = if
					     length(Digits) > 0 ->
						     {true, Line};
					     true ->
						     {false, T}
				     end,
			number_indices(Line2, Digits, Indices, Index2, Halt);
		{_Int, []} ->
			number_indices(
			  T,
			  lists:append(Digits, [H]),
			  lists:append(Indices, [Index2]),
			  Index2,
			  false
			 )
	end.
number_indices(Line) ->
	number_indices(Line, [], [], 0, false). 
