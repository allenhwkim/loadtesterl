-module(loadtest_controller).
-behaviour(gen_server).
-export([start/1, stop/0]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, handle_cast/2, terminate/2]).
-record(state, {start_time, config, stats_file}).
-record(stats, {count, min, max, avg}).

start(ConfigFile) ->
	gen_server:start({global,?MODULE}, ?MODULE, _Args=[ConfigFile], _Options=[]).

stop() ->
	gen_server:call({global,?MODULE}, stop).

%-----------------------------------------------------------------------------------------------------
% gen_server_functions
%-----------------------------------------------------------------------------------------------------
init([ConfigFile]) ->
	ets:new(master_stats, [private, named_table]),	
	{ok, [Configs]} = file:consult(ConfigFile),
	LogDir = make_log_dir(proplists:get_value(log_dir, Configs)),
	{ok, MasterStats} = file:open(LogDir++"/master.csv", write),
	io:format("controller ~p stats will be written to ~p~n", [node(), LogDir++"/master.csv"]),
	{ok, _SlaveNodes} = prepare_runners(Configs),
	{ok, MaxDuration} = start_runners(Configs),
	timer:sleep(100),  % add delay 100ms for stats printer
	StartTime = now(),
	{ok, _} = timer:apply_interval(10000, gen_server, cast, [{global,?MODULE}, print_master_stats]),
	{ok, _} = timer:apply_after(MaxDuration*1000, ?MODULE, stop, []), 
	{ok, #state{start_time=StartTime, config=Configs, stats_file=MasterStats}}.

handle_cast({add_to_master_stats, {From, Stats}}, State) ->
	TimeElapsed = erlang:round(timer:now_diff(now(), State#state.start_time)/1000000),
	io:format("~p sec: Received Stats ~p From ~p~n",[TimeElapsed, Stats,From]),
	lists:foreach( fun({Key, NewStat}) ->
		UpdatedStat = case ets:lookup(master_stats, Key) of
			[{Key, PrevStat}] ->
				%io:format("controller ~p Adding ~p from ~p to Existing ~p~n",[node(), NewStat, From, PrevStat]),
				#stats{
					count=PrevStat#stats.count+NewStat#stats.count,
					min=erlang:min(PrevStat#stats.min, NewStat#stats.min),
					max=erlang:max(PrevStat#stats.max, NewStat#stats.max),
					avg=((PrevStat#stats.count*PrevStat#stats.avg) + (NewStat#stats.count*NewStat#stats.avg)) 
				        / (PrevStat#stats.count + NewStat#stats.count) };
			[] ->
				NewStat
		end,
		ets:insert(master_stats, {Key, UpdatedStat}) 
	end, Stats),
	{noreply, State};
handle_cast(print_master_stats, State) ->
	Stats = ets:tab2list(master_stats),
	Time = timer:now_diff(now(), State#state.start_time) div 1000000,
	lists:foreach( fun({Key, Stat}) ->
		Count = Stat#stats.count,
		Min = Stat#stats.min,
		Max = Stat#stats.max,
		Avg = Stat#stats.avg,
		io:fwrite(State#state.stats_file, "~p,~p,~p,~p,~p,~p~n",[Time,Key,Count,Min,Max,Avg])  
	end, Stats),
	ets:delete_all_objects(master_stats),
	{noreply, State}.

handle_info(Message, State) -> 
	io:format("controller ~p received message~n ~p~n",[node(), Message]),
	{noreply, State}.
handle_call(stop, _From, State) ->
	file:close(State#state.stats_file),
	io:format("controller loadtest_controller stopped~n"),
	init:stop(),
    {stop, normal, stopped, State};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Any, _From, State) ->
	{reply, ignored, State}.
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
terminate(_Reason, _State) -> 
	ok.

%-----------------------------------------------------------------------------------------------------
% private functions
%-----------------------------------------------------------------------------------------------------
make_log_dir(undefined) ->
	{ok, Cwd} = file:get_cwd(),
	make_log_dir(Cwd);
make_log_dir(BaseDir) ->
	{{Y,M,D},{H,I,S}} = calendar:now_to_datetime(now()),
	Wkdir = binary_to_list(list_to_binary(io_lib:format("~p-~p-~p_~p:~p:~p",[Y,M,D,H,I,S]))), 
	file:make_dir(BaseDir++"/log"),
	io:format("controller Making directory ~p~n",[BaseDir++"/log/"++Wkdir]),
	ok = file:make_dir(BaseDir++"/log/"++Wkdir),
	io:format("controller Resetting symliink current to ~p~n",[BaseDir++"/log/"++Wkdir]),
	file:delete(BaseDir++"/log/current"),
	ok = file:make_symlink(BaseDir++"/log/"++Wkdir, BaseDir++"/log/current"),
	BaseDir++"/log/current".

prepare_runners(Configs) ->
	DefaultMFA = proplists:get_value(mfa, Configs, [?MODULE, module_info,[]]),
	RequiredModules = proplists:get_value(required_modules, Configs, [loadtest_runner]),
	RunnerConfigs = proplists:get_value(runners, Configs),
    SlaveNodes=lists:map( fun(RunnerConfig) ->
		Host = proplists:get_value(host,RunnerConfig),
		{LoadtestModule, _F, _A} = proplists:get_value(mfa, RunnerConfig, DefaultMFA),
		Args = "-rsh ssh -setcookie "++atom_to_list(erlang:get_cookie()),
		io:format("controller ~p Starting a slave node ~p with Args ~p ~n",[node(), Host, Args]),
		{ok, SlaveNode} = slave:start_link(Host, "loadtest", Args),
		pong=net_adm:ping(SlaveNode),
		Modules = RequiredModules++[LoadtestModule],
		[ ok=rpc_load_module(SlaveNode, M) || M <- Modules ],
		io:format("controller ~p loaded module(~p) to slave node ~p.~n",[node(), Modules, SlaveNode]),
		SlaveNode
	end, RunnerConfigs),
	{ok, SlaveNodes}.

start_runners(Configs) ->
	DefaultDuration = proplists:get_value(duration, Configs, 60),
	DefaultMFA = proplists:get_value(mfa, Configs, [?MODULE, module_info,[]]),
	DefaultTPS = proplists:get_value(tps, Configs, 10),
	RunnerConfigs = proplists:get_value(runners, Configs),
	MaxDuration = lists:foldl( fun(RunnerConfig, PrevMax) ->
		Host = proplists:get_value(host,RunnerConfig),
		MFA  =  {Module, Fun, Arg} = proplists:get_value(mfa, RunnerConfig, DefaultMFA),
		TPS =  proplists:get_value(tps, RunnerConfig, DefaultTPS),
		Duration = proplists:get_value(duration, RunnerConfig, DefaultDuration),
		SlaveNode = list_to_atom(lists:concat(['loadtest@',Host])),
		{ok,Pid} = rpc:call(SlaveNode, loadtest_runner, start, [MFA, TPS, Duration], 1000), 
		io:format("controller ~p started ~p:~p(~p) with ~p TPS for ~p seconds. pid is ~p ~n",[SlaveNode, Module, Fun, Arg, TPS, Duration, Pid]),
		erlang:max(Duration,PrevMax)
	end, 0, RunnerConfigs),
	{ok, MaxDuration}.

rpc_load_module(Node, Module) ->
	code:soft_purge(Module),
	code:load_file(Module),
	{_Module,Binary,Filename} = code:get_object_code(Module),
	{module,Module} = rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
	ok.

