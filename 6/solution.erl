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
