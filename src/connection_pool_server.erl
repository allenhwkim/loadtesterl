-module(connection_pool_server).
-behaviour(gen_server).
-record(state, {idles=[], actives=[], command_to_connect}).
-export([start/0, stop/0, set/1, status/0, connect/0, close/1]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, handle_cast/2, terminate/2]).

%-----------------------------------------------------------------------------------------------------
% user functions
%-----------------------------------------------------------------------------------------------------
start() ->
	gen_server:start({local,?MODULE}, ?MODULE, _Args=[], _Options=[]).

stop() ->
	gen_server:call(?MODULE, stop).

set(CommandToConnect) -> % {module, function, argument} to connect to a server
	gen_server:call(?MODULE, {set, CommandToConnect}).

status() ->
	gen_server:call(?MODULE, status).

connect() ->
	gen_server:call(?MODULE, connect).

close(Conn) ->
    gen_server:call(?MODULE, {close, Conn}).		

%-----------------------------------------------------------------------------------------------------
% gen_server_functions
%-----------------------------------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{idles=[], actives=[]}}.

handle_call({set,Tuple}, _From, State) ->
	{Reply, NewState} = case Tuple of
		{command_to_connect, {M,F,A}} ->
			{ok, State#state{command_to_connect = {M,F,A}}};
		_ ->
			{ignored, State}
	end,
	{reply, Reply, NewState};
	
handle_call(status, _From, State) ->
	IdleTotal = length(State#state.idles),
	ActiveTotal = length(State#state.actives),
	Total = IdleTotal + ActiveTotal,
	{reply, {Total,{IdleTotal,ActiveTotal}, State}, State};

handle_call(connect, {FromPid, _Tag}, State) when State#state.idles == [] -> % when all connections are active
	{M,F,A} = State#state.command_to_connect,
	{ok, Conn} = apply(M,F,A),  % initiate a new connection
	erlang:link(FromPid),
	{reply, {ok, Conn}, State#state{ actives=State#state.actives++[{FromPid,Conn}] } };

handle_call(connect, {FromPid, _Tag}, State) -> 
	erlang:link(FromPid),
	{M,F,A} = State#state.command_to_connect,
	[Conn|RestOfIdles] = State#state.idles,
	NewState = case erlang:is_process_alive(Conn) of
			true ->
				State#state{ idles=RestOfIdles, actives=State#state.actives++[{FromPid,Conn}] };
			false ->
				{ok, NewConn} = apply(M,F,A),  % initiate a new connection
				State#state{ idles=RestOfIdles, actives=State#state.actives++[{FromPid,NewConn}] }  
		end,
	ConnPid = proplists:get_value(FromPid, State#state.actives),
	{reply, {ok, ConnPid}, NewState};

handle_call({close, Conn}, {FromPid, _Tag}, State) ->
	NewIdles = State#state.idles ++ [Conn],
	NewActives = State#state.actives -- [{FromPid, Conn}],
	NewState = 	State#state{ idles=NewIdles, actives=NewActives },
	{reply, ok, NewState};
	
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(ignore, _From, State) ->
	{reply, ignored, State}.

handle_info({'EXIT', FromPid, _Reason}, State) ->  % need to kill all connections from the FromPid
	erlang:unlink(FromPid),
	AllActives = proplists:lookup_all(FromPid, State#state.actives),
	NewState = lists:foldl( fun({OwnerPid, Conn}, State2) ->
			NewActives = State2#state.actives -- [{OwnerPid, Conn}],
			NewIdles = State2#state.idles ++ [Conn],
			State2#state{ idles=NewIdles, actives=NewActives }
		end, State,  AllActives),
	{noreply, NewState}.

handle_cast(_Request, State) ->
	{noreply, State}.
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
terminate(_Reason, _State) -> 
	ok.

