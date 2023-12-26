-module(solution).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

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
