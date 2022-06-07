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
%% External exports
-export([
	
%	 create_pod/2,
%	 delete_pod/2,
	 %connect/2,
	 started_pods/1,
	 failed_pods/1,
	 
%	 create_cluster/3,
	 delete_cluster/0,
	 read_state/0,

	 boot/0,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		cluster_id=undefined,
		cookie=undefined,
		num_controllers=undefined,
		num_workers=undefined,
		affinity=undefined,
		k3_nodes=undefined,
		controller_start=undefined,
		worker_start=undefined,
		start_time=undefined
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
boot()->
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


started_pods(Type)->
    gen_server:call(?SERVER, {started_pods,Type},infinity).
failed_pods(Type)->
    gen_server:call(?SERVER, {failed_pods,Type},infinity).
    
%%---------------------------------------------------------------
%% Function:delete_pod
%% @doc:delete pod PodNode and PodDir          
%% @param: PodNode
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec delete_cluster()-> ok|{error,term()}.
delete_cluster()->
    gen_server:call(?SERVER, {delete_cluster},infinity).



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
%% Support functions
%
%%---------------------------------------------------------------
%% Function:read_state()
%% @doc: read theServer State variable      
%% @param: non 
%% @returns:State
%%
%%---------------------------------------------------------------
-spec read_state()-> term().
read_state()->
    gen_server:call(?SERVER, {read_state},infinity).
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

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

    %% Check start parameters
    Env=k3_lib:get_env(),
    ClusterId=proplists:get_value(cluster_id,Env),
    Cookie=proplists:get_value(cookie,Env),
    NumControllers=proplists:get_value(num_controllers,Env),
    NumWorkers=proplists:get_value(num_workers,Env),
    Affinity=proplists:get_value(affinty,Env),
    
    %% Start Needed applications
    LogDir="logs",
    LogFileName="k3.log",
    k3_lib:start_needed_appl(ClusterId,LogDir,LogFileName),

    %% Start leader election to determ leader

 
    %% Create a new cluster   

    K3NodeName=ClusterId++"_k3",
    AllK3Nodes=[{HostName,list_to_atom(K3NodeName++"@"++HostName)}||HostName<-config:host_id_all()],
    {ok,StartResult}=k3_lib:create_cluster(ClusterId,Cookie,NumControllers,NumWorkers,Affinity,AllK3Nodes),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{ClusterId," Cluster successfully created"}),
	 
    {StartedControllers,FailedControllers}=proplists:get_value(controllers,StartResult),    
    {StartedWorkers,FailedWorkers}=proplists:get_value(workers,StartResult),
  
  
    {ok, #state{
	    cluster_id=ClusterId,
	    cookie=Cookie,
	    num_controllers=NumControllers,
	    num_workers=NumWorkers,
	    affinity=Affinity,
	    k3_nodes=AllK3Nodes,
	    controller_start={StartedControllers,FailedControllers},
	    worker_start={StartedWorkers,FailedWorkers},
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
handle_call({started_pods,controller},_From, State) ->
    {Reply,_FailedControllers}=State#state.controller_start,
    {reply, Reply, State};
handle_call({failed_pods,controller},_From, State) ->
    {_StartedControllers,Reply}=State#state.controller_start,
    {reply, Reply, State};

handle_call({started_pods,worker},_From, State) ->
    {Reply,_FailedWorkers}=State#state.worker_start,
    {reply, Reply, State};
handle_call({failed_pods,worker},_From, State) ->
    {_StartedWorkers,Reply}=State#state.worker_start,
    {reply, Reply, State};


handle_call({delete_cluster},_From, State) ->
    Reply=ok,
    NewState=State#state{
	       num_controllers=undefined,
	       num_workers=undefined,
	       affinity=undefined,
	       start_time=undefined},
    {reply, Reply, NewState};

handle_call({read_state},_From, State) ->
    Reply=State,
    {reply, Reply, State};

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
handle_info(_Info, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched info",[Info])]),
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

		  
