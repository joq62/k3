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
	 create_cluster/3,
	 delete_cluster/0,
	 read_state/0,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		name=undefined,
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
%% Function:create_cluster
%% @doc:creates a new cluster on       
%% @param: Name,Cookie,NumControllers,Affinity
%% @returns:ok|error,Reason}
%%
%%---------------------------------------------------------------
-spec create_cluster(integer(),integer(),list())-> ok|{error,term()}.
create_cluster(NumControllers,NumWorkers,Affinity)->
    gen_server:call(?SERVER, {create_cluster,NumControllers,NumWorkers,Affinity},infinity).
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

    CommonR=application:start(common),
    ok=application:start(nodelog),
    nodelog_server:create(?LogDir),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start common ",CommonR}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start sd_app ",application:start(sd_app)}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start config_app ",application:start(config_app)}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,"server successfully started"),    

    %% Start leader election to determ leader
    
    %% Check start parameters
    {ok,ClusterIdAtom}=application:get_env(clusterid),
    ClusterId=atom_to_list(ClusterIdAtom),

    {ok,NumControllers}=application:get_env(num_controllers),
    true=is_integer(NumControllers),

    {ok,NumWorkers}=application:get_env(num_workers),
  %  NumWorkers=list_to_integer(atom_to_list(NumWorkersAtom)),
    true=is_integer(NumWorkers),

    {ok,AffinityAtom}=application:get_env(affinity),
    true=is_list(AffinityAtom),
    Affinity=[atom_to_list(Host)||Host<-AffinityAtom],

    Cookie=erlang:get_cookie(),

    K3NodeName=ClusterId++"_k3",
    AllK3Nodes=[{HostName,list_to_atom(K3NodeName++"@"++HostName)}||HostName<-config:host_id_all()],
   % K3Nodes=[{Node,net_adm:ping(Node)}||Node<-AllK3Nodes],


    {ok,StartResult}=k3_lib:create_cluster(ClusterId,Cookie,NumControllers,NumWorkers,Affinity,AllK3Nodes),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{ClusterId," Cluster successfully created"}),	 
    ControllerStart=proplists:get_value(controller_start,StartResult),
    WorkerStart=proplists:get_value(worker_start,StartResult),
  
    {ok, #state{
	    name=ClusterId,
	    cookie=Cookie,
	    num_controllers=NumControllers,
	    num_workers=NumWorkers,
	    affinity=Affinity,
	    k3_nodes=AllK3Nodes,
	    controller_start=ControllerStart,
	    worker_start=WorkerStart,
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
handle_call({create_cluster,NumControllers,NumWorkers,Affinity},_From, State) ->
    ClusterName=State#state.name,
    Cookie=State#state.cookie,

    Reply=case k3_lib:create_cluster(ClusterName,Cookie,NumControllers,NumWorkers,Affinity,State#state.k3_nodes) of
	      {ok,StartResult}->
		  nodelog_server:log(notice,?MODULE_STRING,?LINE,{ClusterName," Cluster successfully created"}),	 
		  ControllerStart=proplists:get_value(controller_start,StartResult),
		  WorkerStart=proplists:get_value(worker_start,StartResult),
		  NewState=State#state{name=ClusterName,
				       cookie=Cookie,
				       num_controllers=NumControllers,
				       affinity=Affinity,
				       controller_start=ControllerStart,
				       worker_start=WorkerStart,
				       start_time={date(),time()}},
		  {ok,StartResult};
	      {error,Reason}->
		  nodelog_server:log(warning,?MODULE_STRING,?LINE,{"error creating Cluster",{error,Reason}}),
		  NewState=State,
		  {error,Reason}
	  end,
    {reply, Reply, NewState};

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

		  
