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

lsurround([]) -> [];
lsurround(List) ->
	Prefix = lists:nth(1, List) - 1,
	if
		Prefix > 0 ->
			lists:append([Prefix], List);
		true ->
			List
	end.

rsurround(List, Max, Max) ->
	List;
rsurround(List, _Max, _Length) ->
	lists:append(List, [lists:nth(length(List), List)+1]).
rsurround([], _Max) -> [];
rsurround(List, Max) ->
	rsurround(List, Max, length(List)).

surround(List, Max) -> lsurround(rsurround(List, Max)).

part_numbers(_PreviousLine, _Line, _NextLine, [], Numbers) ->
	Numbers;
part_numbers(PreviousLine, Line, NextLine, ToProcess, Numbers) ->
	[{Number, Indices}|T] = ToProcess,
	Predicate = fun(E) -> symbol_in_indices(E, Indices) end,
	Numbers2 = case lists:any(Predicate, [PreviousLine, Line, NextLine]) of
			   true -> lists:append([Number], Numbers);
			   false -> Numbers
		   end,
	part_numbers(PreviousLine, Line, NextLine, T, Numbers2).

part_numbers(PreviousLine, Line, NextLine) ->
	part_numbers(PreviousLine, Line, NextLine, all_number_indices(Line), []).
