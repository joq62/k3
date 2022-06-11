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
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start sd_app ",application:start(sd)}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start config_app ",application:start(config)}),
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
create(N,ClusterName,Cookie,Type,Started,_Failed)->
    UniqueString=integer_to_list(erlang:system_time(microsecond),36),
    NodeName=ClusterName++"_"++Type++integer_to_list(N)++"_"++UniqueString,
    NodeDir=filename:join(ClusterName,NodeName),
    ok=file:make_dir(NodeDir),
    {ok,HostName}=net:gethostname(),
    PaArgs=" ",
    EnvArgs=" ",
    
    {ok,Node}=node_server:create(HostName,NodeDir,NodeName,Cookie,PaArgs,EnvArgs),
    true=erlang:monitor_node(Node,true),
    
    %% start commonm
    GitPathCommon=config_server:application_gitpath("common.spec"),
    StartCmdCommon=config_server:application_start_cmd("common.spec"),
    {ok,"common","0.1.0",_ApplDirCommon}=node_server:load_start_appl(Node,NodeDir,"common","0.1.0",GitPathCommon,StartCmdCommon),
    %% start nodelog
    GitPathNodelog=config_server:application_gitpath("nodelog.spec"),
    StartCmdNodelog=config_server:application_start_cmd("nodelog.spec"),
    {ok,"nodelog","0.1.0",_ApplDirNodelog}=node_server:load_start_appl(Node,NodeDir,"nodelog","0.1.0",GitPathNodelog,StartCmdNodelog),
    ok=file:make_dir(filename:join(NodeDir,"logs")),
    LogFile=filename:join([NodeDir,"logs",NodeName++".log"]),
    rpc:call(Node,nodelog_server,create,[LogFile],5000),

    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Started common"}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Started nodelog"}),
   %% start sd
    GitPathSd=config_server:application_gitpath("sd.spec"),
    StartCmdSd=config_server:application_start_cmd("sd.spec"),
    {ok,"sd","0.1.0",_ApplDirSd}=node_server:load_start_appl(Node,NodeDir,"sd","0.1.0",GitPathSd,StartCmdSd),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Started sd"}),	 
    NewStarted=[[{type,list_to_atom(Type)},{node_name,NodeName},{node_dir,NodeDir},{node,Node},{created,{date(),time()}}]|Started],
    NewFailed=[],
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

    


