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
number_indices(Line, Offset) ->
	number_indices(Line, [], [], Offset, false). 

all_number_indices([], NumberPairs, _Offset) ->
	NumberPairs;
all_number_indices(Line, NumberPairs, Offset) ->
	{Line2, Int, Indices} = number_indices(Line, Offset),
	case {Int, Indices} of
		{0, []} ->
			all_number_indices(Line2, NumberPairs, Offset);
		{_, _} ->
			all_number_indices(
			  Line2, 
			  lists:append(NumberPairs, [{Int, Indices}]),
			  lists:last(Indices)
			 )
	end.
all_number_indices(Line) ->
	all_number_indices(Line, [], 0).

% A symbol is anything that is not a dot and not an integer.
is_symbol([$.]) -> false;
is_symbol(S) ->
	case string:to_integer(S) of
		{error, _} -> true;
		{_, _} -> false
	end.

symbol_in_indices(_Line, []) -> false;
symbol_in_indices(Line, [Index|Indices]) ->
	case is_symbol([lists:nth(Index, Line)]) of
		true -> true;
		false -> symbol_in_indices(Line, Indices)
	end.
