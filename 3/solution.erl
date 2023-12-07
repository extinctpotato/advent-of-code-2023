-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

number_indices(Line, Digits, Indices, _Index, true) ->
	{Line, Digits, Indices};
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
		{Int, []} ->
			number_indices(
			  T,
			  lists:append(Digits, [Int]),
			  lists:append(Indices, [Index2]),
			  Index2,
			  false
			 )
	end.
number_indices(Line) ->
	number_indices(Line, [], [], 0, false). 
