-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

possible_sets([]) ->
	true;

possible_sets(Sets) ->
	[Set|OtherSets] = Sets,
	case possible_set(Set) of
		false -> false;
		true ->
			possible_sets(OtherSets)
	end.

possible_set([]) ->
	true;

possible_set(TokenizedSet) ->
	[Tokens|Rest] = TokenizedSet,
	case enough_balls(Tokens) of
		false -> false;
		true ->
			possible_set(Rest)
	end.

enough_balls({X, blue}) ->
	X =< 14;

enough_balls({X, green}) ->
	X =< 13;

enough_balls({X, red}) ->
	X =< 12.

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
	case possible_sets(Sets) of
		false -> 0;
		true -> Game
	end.

process_lines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof -> Acc;
		Line -> process_lines(Device, Acc+parse_line(string:chomp(Line)))
	end.

process_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	process_lines(Device, 0).
