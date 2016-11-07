-module(bank_fsm).

-behaviour(gen_fsm).

-import([init/1,  handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


autorize(Name, Pin) ->
	gen_fsm:sync_send_event(?SERVER, {authorized, Name, Pin}).

init([]) ->	
	{ok, unauthorized, nobody}.


unauthorized({authorize, Name, Pin}, _From, State) ->
	
