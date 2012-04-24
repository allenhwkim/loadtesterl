-module(plotter).
-compile(export_all).

get_plots() ->
	get_plots(".").
get_plots(LogDir) ->
	{ok, PlotDataJs} = file:open(LogDir ++ "/plot_data.js",write),
	{ok, StatsFile} = file:open(LogDir ++ "/master.csv", read),
	Result = read_and_process_line(StatsFile,[]),
	io:format(PlotDataJs, "plotData = {~n", []),
	lists:foreach( fun({Key, Tuple}) ->
		io:format(PlotDataJs, "\"~p\":~n~s,~n", [Key, flot_data(Tuple)])
	end, Result),
	io:format(PlotDataJs, "\"end\":\"end\"~n", []),
	io:format(PlotDataJs, "};~n", []),
	ok=file:close(StatsFile),
	ok=file:close(PlotDataJs),
	ok.
	
read_and_process_line(IoDevice, Result) ->
	case file:read_line(IoDevice) of
		{ok, LineData} ->
			Data = re:replace(LineData,"\n","",[{return,list}]),
			NewResult = process_line(Data, Result),
			read_and_process_line(IoDevice, NewResult);
		eof ->
			Result
	end.

process_line(Data, Result) ->
	[Time, Key, Count, Min, Max, Avg] = lists:map( fun(Str) ->
		case re:run(Str,"[0-9]+([0-9\\.]*)",[{capture,all,list}]) of
			{match,[_,[]]} -> erlang:list_to_integer(Str);
			{match,_}      -> erlang:list_to_float(Str);
			nomatch        -> erlang:list_to_atom(Str)
		end
	end, re:split(Data, ",", [{return,list}]) ),
	if 
		Min==1, Max==1, Avg==1.0 -> %counters
			AllCounters = proplists:get_value(counters, Result,[]),
			PrevCounters = proplists:get_value(Key, AllCounters, []),
			NewCounters = PrevCounters ++ [[Time,(Count/10)]],
			NewAllCounters = lists:keystore(Key, 1, AllCounters, {Key, NewCounters}),
			NewResult = lists:keystore(counters, 1, Result, {counters,NewAllCounters}),
			NewResult;
		true ->
			StatPlots = proplists:get_value(Key, Result,[]),
			%CountPlots = proplists:get_value(count, StatPlots, []) ++ [[Time,Count]],
			MinPlots   = proplists:get_value(min,   StatPlots, []) ++ [[Time,Min]],
			%MaxPlots   = proplists:get_value(max,   StatPlots, []) ++ [[Time,Max]],
			AvgPlots   = proplists:get_value(avg,   StatPlots, []) ++ [[Time,Avg]],
			%NewResult = lists:keystore(Key, 1, Result, {Key,[{count, CountPlots}, {min,MinPlots}, {max, MaxPlots}, {avg, AvgPlots}]}),
			NewResult = lists:keystore(Key, 1, Result, {Key,[{min,MinPlots}, {avg, AvgPlots}]}),
			NewResult
	end.

%[ { label: "min", data : ~p },  { label: "max", data : ~p },  { label: "avg", data : ~p } ];
flot_data(PlotData) ->
	PlotDataJson = lists:map( fun({Label, Plots}) ->
		Formatted = io_lib:format("{\"label\": \"~p\", \"data\": ~p}",[Label, Plots]),
		re:replace(lists:flatten(Formatted),"\\s+","",[global,{return,list}])
	end,PlotData),
	"[" ++ string:join(PlotDataJson, ",\n") ++ "]".
