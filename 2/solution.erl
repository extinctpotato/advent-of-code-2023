-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

possible_set(Red, Green, Blue, TotalRed, TotalGreen, TotalBlue) ->
	(Red >= TotalRed) and (Green >= TotalGreen) and (Blue >= TotalBlue).

enough_balls({X, blue}) ->
	X >= 14;

enough_balls({X, green}) ->
	X >= 13;

enough_balls({X, red}) ->
	X >= 12.

parse_token(Token) ->
	[Count, Color] = string:split(Token, " "),
	{list_to_integer(Count), list_to_atom(Color)}.

parse_tokens([], Acc) ->
	Acc;

parse_tokens(Tokens, Acc) ->
	[Token|OtherTokens] = Tokens,
	parse_tokens(OtherTokens, lists:append(Acc, [parse_token(Token)])).

parse_tokens(Tokens) ->
	parse_tokens(Tokens, []).

parse_set(Set) ->
	Tokens = string:split(Set, ", ", all),
	parse_tokens(Tokens).

parse_sets([], Acc) ->
	Acc;

parse_sets(Sets, Acc) ->
	[Set|OtherSets] = string:split(Sets, "; "),
	parse_sets(OtherSets, lists:append(Acc, [parse_set(Set)])).

parse_sets(Sets) ->
	parse_sets(Sets, []).

parse_line(Line) ->
	[GameL|Tail] = string:split(Line, ": "),
	Game = list_to_integer(lists:last(string:split(GameL, "Game "))),
	Sets = parse_sets(Tail),
	{Game, Sets}.

%process_lines(Device) ->
%	case io:get_line(Device, "") of
%		eof -> smth();
%		Line -> smth_else()
%	end.

process_lines(Device) ->
	[].

process_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	process_lines(Device).
