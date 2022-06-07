%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(k3_lib).  
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 start_controllers/1,
	 start_needed_appl/3,
	 get_env/0,
	 
	 create_cluster/6,
	 create_vm/5,
	 delete_vm/1,
	 delete_vm/2,
	 git_load/4
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================

start_controllers(StartedControllers)->
    ApplId="k3_controller",
    ApplVsn="latest",
    
    start_controllers(StartedControllers,ApplId,ApplVsn,[]),
    ok.

start_controllers([],_,_,Result)->
    Result;
start_controllers([PodInfo|T],ApplId,ApplVsn,Acc) ->
    PodNode=proplists:get_value(pod_node,PodInfo),
    PodDir=proplists:get_value(pod_dir,PodInfo),
    R=pod_lib:load_start(PodNode,PodDir,ApplId,ApplVsn),
    start_controllers(T,ApplId,ApplVsn,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_needed_appl(ClusterId,LogDir,LogFileName)->
    
    CommonR=application:start(common),
    ok=application:start(nodelog),

    os:cmd("rm -rf "++ClusterId),
    ok=file:make_dir(ClusterId),
    ok=file:make_dir(filename:join(ClusterId,LogDir)),
    LogFile=filename:join([ClusterId,LogDir,LogFileName]),
    nodelog_server:create(LogFile),
    
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start common ",CommonR}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start sd_app ",application:start(sd_app)}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start config_app ",application:start(config_app)}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,"server successfully started"),    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_env()->
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
    [{cluster_id,ClusterId},
     {cookie,Cookie},
     {num_controllers,NumControllers},
     {num_workers,NumWorkers},
     {affinity,Affinity}].


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_cluster(ClusterName,Cookie,NumControllers,NumWorkers,_Affinity,_K3Nodes)->
    os:cmd("rm -rf "++ClusterName),
    Reply=case file:make_dir(ClusterName) of
	      {error,Reason}->
		  {error,Reason};
	      ok->
		  
		  {StartedControllers,FailedControllers}=create(NumControllers,ClusterName,Cookie,"controller",[],[]),
		  {StartedWorkers,FailedWorkers}=create(NumWorkers,ClusterName,Cookie,"worker",[],[]),
	
		  {ok,[{controllers,{StartedControllers,FailedControllers}},
		       {workers,{StartedWorkers,FailedWorkers}}]}
	
	  end,
    Reply.
			  
create(0,_,_,_,Started,Failed)->	  
    {Started,Failed};
create(N,ClusterName,Cookie,Type,Started,Failed)->
    UniqueString=integer_to_list(erlang:system_time(microsecond),36),
    PodId=ClusterName++"_"++Type++integer_to_list(N)++"_"++UniqueString,
    PodDir=filename:join(ClusterName,PodId),
    case pod_lib:create_pod(PodId,PodDir) of
	{ok,PodNode}->
	    true=erlang:monitor_node(PodNode,true),
	    CommonR=pod_lib:load_start(PodNode,PodDir,"common","latest"),
	    pod_lib:load_start(PodNode,PodDir,"nodelog","latest"),
	    ok=file:make_dir(filename:join(PodDir,"logs")),
	    LogFile=filename:join([PodDir,"logs",PodId++".log"]),
	    rpc:call(PodNode,nodelog_server,create,[LogFile],5000),
	    RSd=pod_lib:load_start(PodNode,PodDir,"sd_app","latest"),
	    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start common ",CommonR}),
	    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start sd_app ",RSd}),
    	    NewStarted=[[{type,list_to_atom(Type)},{pod_id,PodId},{pod_dir,PodDir},{pod_node,PodNode},{created,{date(),time()}}]|Started],
	    NewFailed=Failed;
	{error,Reason}->
	    NewFailed=[{Reason,PodId,PodDir,{date(),time()}}|Failed],
	    NewStarted=Started
    end,
    create(N-1,ClusterName,Cookie,Type,NewStarted,NewFailed).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
git_load(AppId,Vsn,GitPath,ServiceDir)->
    AppDir=filename:join(ServiceDir,AppId++"_"++Vsn),
    os:cmd("rm -rf "++AppDir),
    ok=file:make_dir(AppDir),
    TempDir="temp.dir",
    os:cmd("rm -rf "++TempDir),
    ok=file:make_dir(TempDir),
    os:cmd("git clone "++GitPath++" "++TempDir),
    os:cmd("mv  "++TempDir++"/*"++" "++AppDir),
    os:cmd("rm -rf "++TempDir),
    Ebin=filename:join(AppDir,"ebin"),
    Reply=case filelib:is_dir(Ebin) of
	      true->
		  case code:add_patha(Ebin) of
		      true->
			  {ok,AppDir};
		      Err ->
			  {error,[Err]}
		  end;
	      false ->
		  {error,[no_dir_created,?MODULE,?LINE]}
	  end,
    Reply.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
% NodeDir=filename:join(NodeName++".pod"),
create_vm(HostName,NodeName,Cookie,PaArgs,EnvArgs)->
  %  io:format("HostName ~p~n",[HostName]),
  %  io:format("NodeName ~p~n",[NodeName]),
  %  io:format("PaArgs ~p~n",[{PaArgs,?MODULE,?LINE}]),
  %  io:format("Cookie ~p~n",[Cookie]),
  %  io:format("EnvArgs ~p~n",[EnvArgs]),
    

    Args=PaArgs++" "++"-setcookie "++Cookie++" "++EnvArgs,
    Result=case slave:start(HostName,NodeName,Args) of
	       {error,Reason}->
		   {error,[Reason]};
	       {ok,SlaveNode}->
		   case net_kernel:connect_node(SlaveNode) of
		       false->
			   {error,[failed_connect,SlaveNode]};
		       ignored->
			   {error,[ignored,SlaveNode]};
		       true->
			   {ok,SlaveNode}
		   end
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	       
delete_vm(Node)->
    slave:stop(Node).

delete_vm(Node,Dir)->
    slave:stop(Node),
    os:cmd("rm -rf "++Dir),
    timer:sleep(500),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
 

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

    


