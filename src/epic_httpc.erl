-module(epic_httpc).
-compile(export_all).
-record(url,{protocol,user,password,host,port,path}).
 
get(Url,Headers) ->
	Responses = httpc_request(Url, Headers),
	{processing_time, ConnectionTime} =  lists:keyfind(processing_time, 1, Responses),
	{connection_time, ProcessingTime} =  lists:keyfind(connection_time, 1, Responses),
	{response, Response} =  lists:keyfind(response, 1, Responses),
	StatusCode = get_status_code(Response),
	[
		{latency, erlang:trunc(ProcessingTime/1000)},
		%{connection_time,erlang:trunc(ConnectionTime/1000)},
		{StatusCode,1}
	].

httpc_request(UrlIn,Headers) ->
	Url = parse_url(UrlIn),
	Request = build_request("GET",Url#url.host, Url#url.port, Url#url.path, Headers),
	Responses = get_response(Url#url.host, Url#url.port, Request),
	Responses.
   
get_response(Host,Port,Request) ->
	IntPort = if 
		Port == "" -> 80; 
		true -> list_to_integer(Port)
	end,
	Start = now(),
	ResultConnect =  gen_tcp:connect(Host, IntPort, [list, {packet, 0},{reuseaddr, true},{active,false}]), 
	ConnectionTime = timer:now_diff(now(), Start),
	%io:format("ResultConnnect ~p~n",[ResultConnect]),
	Response = case ResultConnect of
		{ok, Sock} -> 
			ResultSend = gen_tcp:send(Sock, Request),
			%io:format("ResultSend ~p~n",[ResultSend]),
			ResultRecv = case ResultSend of
				ok ->
					RecvResult = gen_tcp:recv(Sock, 0),   % it's only for a very simple tcp communication.
					RecvResult;
				SendError -> 
					SendError
			end,
			(catch gen_tcp:close(Sock)),
			ResultRecv;
		ConnectError ->
			ConnectError	
	end,
	ProcessingTime = timer:now_diff(now(), Start),
	[
		{connection_time, ConnectionTime},
		{processing_time, ProcessingTime},
		{response, Response}
	].
  
build_request("GET", Host, Port, Path, Headers) ->
    AllHeaders = case proplists:get_value("host",Headers) of 
        undefined -> 
			Host =  if 
				Port == "80"; Port=="" -> Host;
				true -> Host ++ ":" ++ Port
			end,
			Headers ++ [{"Host", Host }];
        _ -> Headers
    end,
	FormattedHeaders = lists:map( fun({Key,Value}) ->
		Key++":"++Value++"\r\n"	
	end, AllHeaders),
	[ "GET ", Path, " HTTP/1.1\r\n", FormattedHeaders++"\r\n" ].
 
parse_url(Url) ->
	{ok,RE}= re:compile("^(http[s]?://)?([a-z0-9]+(:[^@]+)?@)?([a-z0-9\.\-]+(:[0-9]+)?)([/].*)?$",[caseless]),
	case re:run(Url,  RE, [global, {capture, all, list}]) of
		{match,[[_Match,Scheme,UserPass,Password,Host]]} ->
			Protocol = re:replace(Scheme,"://","",[{return,list}]),
			User = re:replace(UserPass,Password++"@","",[{return,list}]),
			#url{protocol=Protocol,user=User,password=Password,host=Host,port="80",path=""};
		{match,[[_Match,Scheme,UserPass,Password,UrlHost,UrlPort]]} ->
			Protocol = re:replace(Scheme,"://","",[{return,list}]),
			User = re:replace(UserPass,Password++"@","",[{return,list}]),
			Host = re:replace(UrlHost,":[0-9]+$","",[{return,list}]),
			Port = re:replace(UrlPort,":","",[{return,list}]),
			#url{protocol=Protocol,user=User,password=Password,host=Host,port=Port,path=""};
		{match,[[_Match,Scheme,UserPass,Password,UrlHost,UrlPort,Path]]} ->
			Protocol = re:replace(Scheme,"://","",[{return,list}]),
			User = re:replace(UserPass,Password++"@","",[{return,list}]),
			Host = re:replace(UrlHost,":[0-9]+$","",[{return,list}]),
			Port = re:replace(UrlPort,":","",[{return,list}]),
			#url{protocol=Protocol,user=User,password=Password,host=Host,port=Port,path=Path};
		nomatch -> 
			#url{}
	end.

get_status_code(Response) ->
	StatusCode = case Response of
		{ok, ResponseStr}  ->
			case re:run(ResponseStr, "^([A-Z]+\/[0-9\.]+) ([0-9]+) ([A-Z]+)",[{capture, all, list}]) of 
				{match, [_Match,_Version,Status,_OK]} -> 
					list_to_integer(Status);
				nomatch ->
					{error, invalid_response}
			end;
		Error ->
			Error
	end,
	StatusCode.
