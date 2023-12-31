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

all_symbol_indices([], Indices, _Index) ->
	Indices;
all_symbol_indices([Char|Line], Indices, Index) ->
	all_symbol_indices(
	  Line,
	  case is_symbol([Char]) of
		  true -> lists:append(Indices, [Index]);
		  false -> Indices
	  end,
	  Index+1
	 ).
all_symbol_indices(Line) ->
	all_symbol_indices(Line, [], 1).

% A symbol is anything that is not a dot and not an integer.
is_symbol([]) -> false;
is_symbol([$.]) -> false;
is_symbol(S) ->
	case string:to_integer(S) of
		{error, _} -> true;
		{_, _} -> false
	end.

symbol_in_indices([], _Indices) -> false;
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
			[Prefix];
		true ->
			[]
	end.

rsurround([], _Max) -> [];
rsurround(List, Max) ->
	Suffix = lists:nth(length(List), List) + 1,
	if
		Suffix > Max ->
			[];
		true ->
			[Suffix]
	end.

surround(List, Max) ->
	lists:append(lists:append(lsurround(List), List), rsurround(List, Max)).

surrounded_pairs(Lines) ->
	lists:flatten(lists:map(
			fun(L) -> lists:map(
				    fun({N,I}) -> {N, surround(I, length(L))} end,
				    all_number_indices(L)
				   )
			end,
			Lines
		       )).

gears(_SymbolIndex, [], [_Gear1]) ->
	[];
gears(_SymbolIndex, [], [Gear1,Gear2]) ->
	[Gear1,Gear2];
gears(SymbolIndex, [{Number, Indices}|Pairs], Gears) ->
	gears(
	  SymbolIndex,
	  Pairs,
	  case lists:member(SymbolIndex, Indices) of
		  true -> [Number|Gears];
		  false -> Gears
	  end
	 ).

gears(SymbolIndex, NumberPairs) ->
	gears(SymbolIndex, NumberPairs, []).

gear_ratios2(_Pairs, [], Ratios) ->
	Ratios;
gear_ratios2(Pairs, [SymbolIndex|SymbolIndices], Ratios) ->
	gear_ratios2(
	  Pairs,
	  SymbolIndices,
	  case gears(SymbolIndex, Pairs) of
		  [Gear1,Gear2] -> [Gear1 * Gear2|Ratios];
		  [] -> Ratios
	  end
	 ).

gear_ratios(PreviousLine, Line, NextLine) ->
	gear_ratios2(
	  surrounded_pairs([PreviousLine, Line, NextLine]), 
	  all_symbol_indices(Line),
	  []
	 ). 

part_numbers(_PreviousLine, _Line, _NextLine, [], Numbers) ->
	Numbers;
part_numbers(PreviousLine, Line, NextLine, ToProcess, Numbers) ->
	[{Number, Indices}|T] = ToProcess,
	Predicate = fun(E) -> symbol_in_indices(E, surround(Indices, length(Line))) end,
	Numbers2 = case lists:any(Predicate, [PreviousLine, Line, NextLine]) of
			   true -> lists:append([Number], Numbers);
			   false -> Numbers
		   end,
	part_numbers(PreviousLine, Line, NextLine, T, Numbers2).

part_numbers(PreviousLine, Line, NextLine) ->
	part_numbers(PreviousLine, Line, NextLine, all_number_indices(Line), []).

%%% Tests

gears_test_() ->
	Line1 = "467..114..",
	Line2 = "...*......",
	Line3 = "..35..633.",
	Pairs = surrounded_pairs([Line1, Line2, Line3]),
	FirstSymbol = lists:nth(1, all_symbol_indices(Line2)),
	Gears = gears(FirstSymbol, Pairs),
	GearRatios = gear_ratios(Line1, Line2, Line3),
	[?_assert(FirstSymbol =:= 4),
	 ?_assert(length(Gears) =:= 2),
	 ?_assert(lists:nth(1, Gears) =:= 35),
	 ?_assert(lists:nth(2, Gears) =:= 467),
	 ?_assert(lists:nth(1, GearRatios) =:= 16345)
	].

input_test_() ->
	Path = "input.txt",
	[?_assert(process_file(Path, first_part) =:= 527364),
	 ?_assert(process_file(Path, second_part) =:= 79026871)
	].

%%%

process_lines(_Device, Acc, _Line1, [], _Fun) ->
	Acc;
process_lines(Device, Acc, Line1, Line2, Fun) ->
	Line3 = case io:get_line(Device, "") of
			eof -> [];
			Line -> string:chomp(Line)
		end,
	Acc2 = Acc + lists:sum(Fun(Line1, Line2, Line3)),
	process_lines(Device, Acc2, Line2, Line3, Fun).

process_lines(Device, Acc, Fun) ->
	Line = string:chomp(io:get_line(Device, "")),
	process_lines(Device, Acc, [], Line, Fun).

process_file(Path, TaskPart) ->
        {_, Device} = file:open(Path, [read]),
        process_lines(
	  Device,
	  0,
	  case TaskPart of
		  first_part -> fun(L1,L2,L3) -> part_numbers(L1, L2, L3) end;
		  second_part -> fun(L1,L2,L3) -> gear_ratios(L1, L2, L3) end
	  end
	 ).
