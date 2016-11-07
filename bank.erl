-module(bank).

-behaviour(gen_server).

-export([start/0, create_account/2, stop/0, delete_account/1, authorize/2, deposit/2]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, 
terminate/2, code_change/3]).

%%%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

create_account(Name, Pin) ->
    gen_server:call(?MODULE, {create, Name, Pin}).

delete_account(Name) ->
    gen_server:call(?MODULE, {remove, Name}).

deposit(Name, Amount) ->
		gen_server:call(?MODULE, {deposit, Name, Amount}).

authorize(Name, Pin) ->
		gen_server:call(?MODULE, {auth, Name, Pin}).
%%%

init([]) ->
    {ok, dict:new()}.

handle_call({deposit, Name, _Amount}, _From, State) ->
		case dict:find(Name, State) of
			{ok, Value} ->
				{reply, Value, State};
			error ->
				{reply, wrong_account, State}
		end;


handle_call({auth, Name, Pin}, _From, State) ->
		case dict:find(Name, State) of
		{ok, Value} ->
					{{amount, _Some}, {pin, Like_Pin}} = Value,
						if
						  Pin == Like_Pin ->
									{reply, ok, State};
							Pin =/= Like_Pin ->
									{reply, {error, wrond_pin}, State}	
						end;
		error  ->
					{reply, {error, wrong_account}, State}
		end;

handle_call({create, Name, Pin}, _From, State) ->
    case dict:is_key(Name, State) of
        true ->
            {reply, already_exists, State};
        false ->
            {reply, created, dict:store(Name, {{amount, 0}, {pin, Pin}}, State)}
    end;

handle_call({remove, Name}, _From, State) ->
    case dict:is_key(Name, State) of
				true -> 
            {reply, deleted, dict:erase(Name, State)};
				false ->
						{reply, not_exists, State}
	 end;

handle_call(_Request, _From, State) ->
    {reply, invalid_message, State}.

handle_cast(stop, State) ->
    io:format("Normall stop~n"),
    {stop, normal, State};
    
handle_cast(Request, State) ->
    {reply, Request, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
		io:format(dict:to_list(State)),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
