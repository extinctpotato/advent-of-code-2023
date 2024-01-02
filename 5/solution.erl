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

parse_line(Line, {[],[]}, []) -> {parse_seeds(Line), [], []};  % parse first line
parse_line("", {Seeds, []}, []) -> {Seeds, [], []};            % parse second line
parse_line("", {Seeds, Maps}, Acc) -> {Seeds, [lists:reverse(Acc)|Maps], []}; % empty line
parse_line(Line, {Seeds, Maps}, []) -> {Seeds, Maps, [parse_map_header(Line)]};

parse_line(Line, {Seeds, Maps}, Acc) ->
	{Seeds, Maps, [parse_numbers(Line)|Acc]}.

process_lines(Device, Acc) ->
	{Seeds, Maps, LineAcc} = Acc,
	case io:get_line(Device, "") of
		% parse_line/3 doesn't know that there are no more lines to chug
		% so we have to simulate a bogus empty line to flush the line accumulator.
		% Additionally, we reverse the maps list so that the order matches that
		% of the file and we reject the (assumed to be empty) accumulator.
		eof -> 
			{Seeds2, Maps2, []} = parse_line("", {Seeds, Maps}, LineAcc),
			{Seeds2, lists:reverse(Maps2)};
		L -> 
			process_lines(
			  Device, 
			  parse_line(string:chomp(L), {Seeds,Maps}, LineAcc)
			 )
	end.

process_file(Path) ->
	{_, Device} = file:open(Path, [read]),
	process_lines(Device, {[],[],[]}).

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
