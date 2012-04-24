-module(my_module).
-compile(export_all).

my_fun(SleepTime)->
	T1 = now(),
	timer:sleep(SleepTime),                %think it as your function processing time
	Latency=timer:now_diff(now(),T1),
	{_,_,Ms} = now(),
	RandomCase = case Ms rem 3 of
		0 -> zero;
		1 -> one;
		2 -> two
	end,
	[{latency,Latency}, {RandomCase, 1}].  % need to retrun a proplists
