%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% load,start,stop unload applications in the pods vm
%%% supports with services
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(k3_server).  

-behaviour(gen_server).  

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(LogDir,"logs").
-define(DeplSpecExtension,".depl_spec").
-define(Interval,30*1000).

%% External exports
-export([
	 desired_state_check/0,

	 appl_start/1,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		deployment_name,
		start_time=undefined
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
appl_start([])->
    application:start(k3).

%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================


%%---------------------------------------------------------------
%% Function:template()
%% @doc: service spec template  list of {app,vsn} to run      
%% @param: 
%% @returns:[{app,vsn}]
%%
%%---------------------------------------------------------------
%-spec template()-> [{atom(),string()}].
%template()->
 %   gen_server:call(?SERVER, {template},infinity).


%% ====================================================================

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


desired_state_check()->
    gen_server:cast(?SERVER, {desired_state_check}).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |

%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    
    {ok,DeploymentName}=application:get_env(deployment_name),
    rpc:cast(node(),nodelog_server,log,[notice,?MODULE_STRING,?LINE,
					{"OK, started server at node  ",?MODULE," ",node()}]),
    spawn(fun()->local_desired_state_check(DeploymentName) end),
    
    {ok, #state{
	    deployment_name=DeploymentName,
	    start_time={date(),time()}
	   }
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------



handle_cast({desired_state_check}, State) ->
    spawn(fun()->local_desired_state_check(State#state.deployment_name) end),
    {noreply, State};

handle_cast(_Msg, State) ->
  %  rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({nodedown,Node}, State) ->
    io:format(" ~p~n",[{?MODULE,?LINE,nodedown,Node}]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

local_desired_state_check(DeploymentName)->		  
    timer:sleep(?Interval),
    case leader_server:am_i_leader(node()) of
	true->
	    rpc:call(node(),k3_orchistrate,desired_state,[DeploymentName],20*1000);
	false ->
	    ok
    end,
    rpc:cast(node(),k3_server,desired_state_check,[]).
