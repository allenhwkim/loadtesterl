-module(loadtest_runner).
-behaviour(gen_server).
-export([start/3, stop/0, run/3, spawn_mfa/5]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, handle_cast/2, terminate/2]).
-record(state, {mfa, tps, duration}).
-record(stats, {count=0, min=999999, max=0, avg=0}).

start(MFA,TPS,Duration) ->
	gen_server:start({local, ?MODULE}, ?MODULE, _Args=[MFA,TPS,Duration], _Options=[]).

stop() ->
	gen_server:call(?MODULE, stop).

%-----------------------------------------------------------------------------------------------------
% gen_server_functions
%-----------------------------------------------------------------------------------------------------
% spawn processes every second, and send stats every 10 seconds
init([{M,F,A}=MFA,TPS,Duration]) ->
	process_flag(trap_exit,true),
	ets:new(slave_stats, [private, named_table]),
	% start the first run processes(as many as TPS)
	spawn_mfa(M,F,A,0,TPS),
	% then every second, spawn processes(as many as TPS)
	{ok, _SpawnTimer} = timer:apply_interval(1000, ?MODULE, spawn_mfa, [M,F,A,0,TPS]),
	% then every 10 seconds, collects stats and send it to loadtest_congtroller
	{ok, _StatTimer} = timer:apply_interval(10000, gen_server, cast, [?MODULE, send_stats_to_master]),  
	{ok, #state{mfa=MFA, tps=TPS, duration=Duration}}.

handle_cast({add_to_stats, Numbers}, State) when is_list(Numbers) ->
	lists:foreach( fun({Key, Num}) ->
		NewStat = case ets:lookup(slave_stats, Key) of
			[{Key, Value}] ->
				Value#stats{
					count=Value#stats.count+1,
					min=erlang:min(Value#stats.min, Num),
					max=erlang:max(Value#stats.max, Num),
					avg=erlang:trunc( (Value#stats.count*Value#stats.avg+Num)/(Value#stats.count+1)) };
			[] ->
				#stats{count=1, min=Num, max=Num, avg=Num}
		end,
		ets:insert(slave_stats, {Key, NewStat}) 
	end, Numbers),
	{noreply, State};

handle_cast(send_stats_to_master, State) ->
	Stats = ets:tab2list(slave_stats),
	%io:format("~p Send/resetting stats to loadtest_controller ~n ~p~n",[node(), Stats]),
	gen_server:cast({global,loadtest_controller}, {add_to_master_stats, {node(), Stats}}),
	ets:delete_all_objects(slave_stats),
	{noreply, State}.

handle_info(Message, State) -> 
	io:format("~p received message~n ~p~n",[node(), Message]),
	{noreply, State}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Any, _From, State) ->
	{reply, ignored, State}.
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
terminate(_Reason, _State) -> 
	ok.

%-----------------------------------------------------------------------------------------------------
% private functions
%-----------------------------------------------------------------------------------------------------
run(M,F,A) ->
	Numbers = apply(M,F,A),
	gen_server:cast(?MODULE, {add_to_stats, Numbers}).

spawn_mfa(_M,_F,_A,N,TPS) when N >= TPS ->
	ok;
spawn_mfa(M,F,A,N,TPS) ->
	spawn(?MODULE, run, [M,F,A]),
	spawn_mfa(M,F,A,N+1, TPS).

