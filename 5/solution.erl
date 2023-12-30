-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

parse_numbers(Line) ->
	lists:map(
	  fun(E) -> case string:to_integer(E) of
			    {Int, []} -> Int
		    end
	  end,
	  string:split(Line, " ", all)
	 ).

parse_seeds(Line) ->
	{ok, R} = re:compile("seeds:\s((?:\s*[0-9]+)+)"),
	case re:split(Line, R, [{return, list}]) of
		[[], Numbers, []] -> parse_numbers(Numbers)
	end.

parse_map_header(Line) ->
	{ok, R} = re:compile("([a-z]+)\-to\-([a-z]+)\smap:"),
	case re:run(Line, R, [{capture, all, list}]) of
		{match, [Line, Source, Destination]} -> {Source, Destination}
	end.

parse_line(Line, {[],[]}) -> {parse_seeds(Line), []}; % parse first line
parse_line("", {S,[]}) -> {S, []};                    % parse second line

parse_line(Line, {_S, Maps}) ->
	Line. % TODO: implement

process_lines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof -> Acc;
		L -> 
			string:chomp(L),
			process_lines(Device, Acc)
	end.

process_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	process_lines(Device, 0).

%%% Tests

parse_numbers_test_() ->
	[?_assert(parse_numbers(L) =:= V) 
	 || {L,V} <- [
		      {"50 98 2", [50, 98,2]},
		      {"52 50 48", [52, 50, 48]},
		      {"0 15 37", [0, 15, 37]},
		      {"0 69 1", [0, 69, 1]},
		      {"1 0 69", [1, 0, 69]}
		     ]
	].
