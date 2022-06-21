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
	 init_etcd/0,
	 start_k3_on_hosts/3,
	 start_controllers/1,
	 start_needed_appl/3,
	 get_env/0,
	 
	 create_cluster/5
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
init_etcd()->
    ok=etcd:appl_start([]),
    pong=etcd:ping(), 
    ok=etcd:dynamic_db_init([]),
    ok=db_host_spec:init_table(),
    ok=db_application_spec:init_table(),
    ok=db_deployment_info:init_table(),
    ok=db_deployments:init_table(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_k3_on_hosts(ClusterId,CookieStr,Hosts)->
    
    NodeName=ClusterId++"_k3",
    {ok,MyHostName}=net:gethostname(),
    AllHostNames=[HostName||HostName<-Hosts,
			    MyHostName/=HostName],
						      
    PaArgs=" ",
    EnvArgs=" ",
    ClusterDir=ClusterId,
    AllK3Nodes=start_host_vm(AllHostNames,ClusterDir,NodeName,CookieStr,PaArgs,EnvArgs,[]),
 %   {ok,HostNode,HostName}
    
    %% start node on each K3
    ok=git_load_start_node(AllK3Nodes),
    AllK3Nodes.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
git_load_start_node([])->
    ok;
git_load_start_node([{ok,Node,_HostName,ClusterDir}|T])->
    %% copy host_info_specs and deployments
    ok=rpc:call(Node,file,make_dir,[filename:join([ClusterDir,"host_info_specs"])],5000),
    rpc:call(Node,os,cmd,["cp host_info_specs/* "++filename:join([ClusterDir,"host_info_specs"])]),
    ok=rpc:call(Node,file,make_dir,[filename:join([ClusterDir,"deployments"])],5000),
    rpc:call(Node,os,cmd,["cp host_info_specs/* "++filename:join([ClusterDir,"deployments"])]),

    %% git clone application and deployment specs
  _GitPathApplicationInfo=config:deployment_spec_applicatioapplication_gitpath("node.spec"),


    ApplDir=filename:join([ClusterDir,"node"]),
    ok=rpc:call(Node,file,make_dir,[ApplDir],5000),
    GitPath=config:application_gitpath("node.spec"),
    {M,F,A}=config:application_start_cmd("node.spec"),
   
    TempDir="temp"++"_"++ClusterDir++".dir",
    rpc:call(Node,os,cmd,["rm -rf "++TempDir],5000),
    ok=rpc:call(Node,file,make_dir,[TempDir],5000),
    rpc:call(Node,os,cmd,["git clone "++GitPath++" "++TempDir],5000),
    rpc:call(Node,os,cmd,["mv  "++TempDir++"/*"++" "++ApplDir],5000),
    rpc:call(Node,os,cmd,["rm -rf "++TempDir],5000),
    Ebin=filename:join([ApplDir,"ebin"]),
    true= rpc:call(Node,filelib,is_dir,[Ebin],5000),
    true=rpc:call(Node,code,add_patha,[Ebin],5000),
    true=rpc:call(Node,code,add_patha,[ClusterDir],5000),
    true=rpc:call(Node,code,add_patha,[ClusterDir++"/*"],5000),
    ok=rpc:call(Node,M,F,A,2*5000),
    pong=rpc:call(Node,node,ping,[],5000),
    git_load_start_node(T).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

start_host_vm([],_ClusterDir,_NodeName,_CookieStr,_PaArgs,_EnvArgs,AllK3Nodes)->
    AllK3Nodes;
start_host_vm([HostName|T],ClusterDir,NodeName,CookieStr,PaArgs,EnvArgs,Acc)->
    NewAcc=case node:ssh_create(HostName,NodeName,CookieStr,PaArgs,EnvArgs) of
	       {ok,HostNode}->
		   case rpc:call(HostNode,os,cmd,["rm -rf "++ClusterDir],5000) of
		       {badrpc,Reason}->
			   nodelog:log(notice,?MODULE_STRING,?LINE,{"Error reate node at host ",HostName,badrpc,Reason}),
			   Acc;
		       _->
			   case rpc:call(HostNode,file,make_dir,[ClusterDir],5000) of
			       {error,Reason}->
				   nodelog:log(notice,?MODULE_STRING,?LINE,{"Error failed to create node at host ",HostName,Reason}),
				   Acc;
			       ok->
				   nodelog:log(notice,?MODULE_STRING,?LINE,{"Cluster node  successfully created at host ",HostName," ",HostNode}),
				   [{ok,HostNode,HostName,ClusterDir}|Acc]
			   end
		   end;
	       {error,Reason}->
		   nodelog:log(notice,?MODULE_STRING,?LINE,{"Error failed to create node at host ",HostName,Reason}),
		   Acc;
	       Error ->
		   nodelog:log(notice,?MODULE_STRING,?LINE,{"Error reate node at host ",HostName,Error}),
		   Acc
	   end,
    start_host_vm(T,ClusterDir,NodeName,CookieStr,PaArgs,EnvArgs,NewAcc).
    
		   
		   


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


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
    nodelog:create(LogFile),
    
    nodelog:log(notice,?MODULE_STRING,?LINE,{"Result start common ",CommonR}),
    nodelog:log(notice,?MODULE_STRING,?LINE,{"Result start sd_app ",application:start(sd)}),
    nodelog:log(notice,?MODULE_STRING,?LINE,{"Result start config_app ",application:start(config)}),
    nodelog:log(notice,?MODULE_STRING,?LINE,"server successfully started"),    
    ok.

%% ---------------------------------------------------0-----------------
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
    {ok,HostsAtom}=application:get_env(hosts),
    true=is_list(HostsAtom),
    Hosts=[atom_to_list(Host)||Host<-HostsAtom],
    Cookie=erlang:get_cookie(),
    [{cluster_id,ClusterId},
     {cookie,Cookie},
     {num_controllers,NumControllers},
     {num_workers,NumWorkers},
     {hosts,Hosts}].


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_cluster(ClusterName,Cookie,NumControllers,NumWorkers,_K3Nodes)->
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
    
    {ok,Node}=node:create(HostName,NodeDir,NodeName,Cookie,PaArgs,EnvArgs),
    true=erlang:monitor_node(Node,true),
    
    %% start commonm
    GitPathCommon=config:application_gitpath("common.spec"),
    StartCmdCommon=config:application_start_cmd("common.spec"),
    {ok,"common","0.1.0",_ApplDirCommon}=node:load_start_appl(Node,NodeDir,"common","0.1.0",GitPathCommon,StartCmdCommon),
    %% start nodelog
    GitPathNodelog=config:application_gitpath("nodelog.spec"),
    StartCmdNodelog=config:application_start_cmd("nodelog.spec"),
    {ok,"nodelog","0.1.0",_ApplDirNodelog}=node:load_start_appl(Node,NodeDir,"nodelog","0.1.0",GitPathNodelog,StartCmdNodelog),
    ok=file:make_dir(filename:join(NodeDir,"logs")),
    LogFile=filename:join([NodeDir,"logs",NodeName++".log"]),
    rpc:call(Node,nodelog,create,[LogFile],5000),

    nodelog:log(notice,?MODULE_STRING,?LINE,{"Started common"}),
    nodelog:log(notice,?MODULE_STRING,?LINE,{"Started nodelog"}),
   %% start sd
    GitPathSd=config:application_gitpath("sd.spec"),
    StartCmdSd=config:application_start_cmd("sd.spec"),
    {ok,"sd","0.1.0",_ApplDirSd}=node:load_start_appl(Node,NodeDir,"sd","0.1.0",GitPathSd,StartCmdSd),
    nodelog:log(notice,?MODULE_STRING,?LINE,{"Started sd"}),	 
    NewStarted=[[{type,list_to_atom(Type)},{node_name,NodeName},{node_dir,NodeDir},{node,Node},{created,{date(),time()}}]|Started],
    NewFailed=[],
    create(N-1,ClusterName,Cookie,Type,NewStarted,NewFailed).



