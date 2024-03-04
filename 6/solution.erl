-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

boat_distance(HoldFor, RaceDuration) ->
	(RaceDuration - HoldFor) * HoldFor.

% You never let go of the button. The boat can't move until you let go of the button.
ways_to_win({Time, _Distance}, Time, Ways) ->
	Ways;

ways_to_win({Time, Distance}, HoldFor, Ways) ->
	Distance2 = boat_distance(HoldFor, Time),
	Ways2 = if
			Distance2 > Distance -> Ways + 1;
			true -> Ways
		end,
	ways_to_win({Time, Distance}, HoldFor+1, Ways2).

ways_to_win(Race) ->
	ways_to_win(Race, 0, 0).

margin_of_error([], Acc) -> Acc;
margin_of_error([Race|Races], Acc) -> margin_of_error(Races, Acc*ways_to_win(Race)).

margin_of_error(Races) -> margin_of_error(Races, 1).

%%% Text-parsing functions

leading_number_terminal(R, Digits) ->
	case string:to_integer(string:reverse(Digits)) of
		{Int, []} -> {Int, R}
	end.
leading_number([32|R], []) -> leading_number(R, []);
leading_number([32|R], Digits) -> leading_number_terminal(R, Digits);
leading_number([], Digits) -> leading_number_terminal([], Digits);
leading_number([Digit|R], Digits) -> leading_number(R, [Digit|Digits]).

leading_number_all(Line) -> leading_number_all([], Line).
leading_number_all(Numbers, []) -> lists:reverse(Numbers);
leading_number_all(Numbers, Line) ->
	{Number, Line2} = leading_number(Line, []),
	leading_number_all([Number|Numbers], Line2).

parse_line([Title, Numbers]) ->
	{list_to_atom(string:lowercase(Title)), leading_number_all(Numbers)};
parse_line(Line) ->
	parse_line(string:split(Line, ":")).
