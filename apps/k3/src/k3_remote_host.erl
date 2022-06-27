%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(k3_remote_host).    
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 start_k3/1,
	 start_k3/2
%	 start/8
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_k3(HostName,DeploymentName)->
    {ok,ClusterId}=db_deployments:read(name,DeploymentName),
    {ok,CookieStr}=db_deployments:read(cookie,DeploymentName),
    NodeName=ClusterId++"_"++"node",
    PaArgs=" ",
    EnvArgs=" ",
    NodeDirBase=ClusterId,    
    {ok,Node,NodeDir}=create_node(HostName,NodeName,CookieStr,PaArgs,EnvArgs,NodeDirBase),

    ok=common_init(Node,NodeDir),
    ok=etcd_init(Node,NodeDir),
    ok=sd_init(Node,NodeDir),
    ok=nodelog_init(Node,NodeDir),    
    ok=node_init(Node,NodeDir), 
    ok=k3_init(Node,NodeDir,DeploymentName),
    ok=k3_controller_init(Node,NodeDir,DeploymentName),    
    ok=leader_init(Node,NodeDir),  
    {ok,Node,NodeDir,HostName}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

start_k3(DeploymentName)->
    F1 = fun start/2,
    F2= fun start_result/3,
    {ok,ClusterId}=db_deployments:read(name,DeploymentName),
    {ok,CookieStr}=db_deployments:read(cookie,DeploymentName),
    {ok,Hosts}=db_deployments:read(hosts,DeploymentName),
    NodeName=ClusterId++"_"++"node",
    PaArgs=" ",
    EnvArgs=" ",
    Appl="k3.spec",
    NodeDirBase=ClusterId,   
 %   HostNodesStopped=[{list_to_atom(NodeName++"@"++HostName),rpc:call(list_to_atom(NodeName++"@"++HostName),init,stop,[],100)}||HostName<-Hosts],
  %  [HostName|_]=Hosts,
 
  %  L=[{HostName,NodeName,CookieStr,PaArgs,EnvArgs,Appl,NodeDirBase,DeploymentName}],
    
    L=[{HostName,NodeName,CookieStr,PaArgs,EnvArgs,Appl,NodeDirBase,DeploymentName}||HostName<-Hosts],
    true=erlang:set_cookie(node(),list_to_atom(CookieStr)),
    [{StartResult,start_k3}]=mapreduce:start(F1,F2,[],L),
    StartResult.
    
   

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start(Pid,{HostName,NodeName,CookieStr,PaArgs,EnvArgs,_Appl,NodeDirBase,DeploymentName})->
   
    {ok,Node,NodeDir}=create_node(HostName,NodeName,CookieStr,PaArgs,EnvArgs,NodeDirBase),
    ok=common_init(Node,NodeDir),
    ok=etcd_init(Node,NodeDir),
    ok=sd_init(Node,NodeDir),
    ok=nodelog_init(Node,NodeDir),    
    ok=node_init(Node,NodeDir), 
    ok=k3_init(Node,NodeDir,DeploymentName),
    ok=k3_controller_init(Node,NodeDir,DeploymentName),  
    ok=leader_init(Node,NodeDir),  
    Pid!{start_k3,{ok,Node,NodeDir,HostName}}.

start_result(Key,Vals,Acc)->
    [{Vals,Key}|Acc].


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
leader_init(Node,NodeDir)->
    NodeAppl="leader.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
    
    ok=rpc:call(Node,application,set_env,[[{leader,[{application_to_track,k3}]}]],5000),
    {ok,"leader.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,leader,ping,[],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					{"OK, Started application at  node ",leader," ",Node}]),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
k3_controller_init(Node,NodeDir,DeploymentName)->
    NodeAppl="k3_controller.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
    
    ok=rpc:call(Node,application,set_env,[[{k3_controller,[{deployment_name,DeploymentName}]}]],5000),
    {ok,"k3_controller.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,k3_controller,ping,[],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					{"OK, Started application at  node ",k3_controller," ",Node}]),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
k3_init(Node,NodeDir,DeploymentName)->
    NodeAppl="k3.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
    
    ok=rpc:call(Node,application,set_env,[[{k3,[{deployment_name,DeploymentName}]}]],5000),
    {ok,"k3.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,k3,ping,[],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					{"OK, Started application at  node ",k3," ",Node}]),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
node_init(Node,NodeDir)->
    NodeAppl="node.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),

    {ok,"node.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,node,ping,[],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					{"OK, Started application at  node ",node," ",Node}]),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
nodelog_init(Node,NodeDir)->
    NodeAppl="nodelog.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),
    {ok,"nodelog.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,nodelog,ping,[],5000),

    LogDir=filename:join(NodeDir,"logs"),
    LogFileName="cluster.log",
    ok=rpc:call(Node,file,make_dir,[LogDir],5000),
    LogFile=filename:join([LogDir,LogFileName]),
    rpc:call(Node,nodelog,create,[LogFile],5000),   

    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					{"OK, Started application at  node ",nodelog," ",Node}]),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
sd_init(Node,NodeDir)->
    NodeAppl="sd.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),

    {ok,"sd.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,sd,ping,[],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						       {"OK, Started application at  node ",sd," ",Node}]),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
etcd_init(Node,NodeDir)->
    NodeAppl="etcd.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),

    {ok,"etcd.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,etcd,ping,[],5000),
    ok=rpc:call(Node,etcd,dynamic_db_init,[[node()]],5000),
    %io:format("mnesia ~p~n",[rpc:call(Node,mnesia,system_info,[],5000)]),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						       {"OK, Started application at  node ",etcd," ",Node}]),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
common_init(Node,NodeDir)->
    NodeAppl="common.spec",
    {ok,ApplId}=db_application_spec:read(name,NodeAppl),
    {ok,ApplVsn}=db_application_spec:read(vsn,NodeAppl),
    {ok,GitPath}=db_application_spec:read(gitpath,NodeAppl),
    {ok,StartCmd}=db_application_spec:read(cmd,NodeAppl),

    {ok,"common.spec",_,_}=node:load_start_appl(Node,NodeDir,ApplId,ApplVsn,GitPath,StartCmd),
    pong=rpc:call(Node,common,ping,[],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						       {"OK, Started application at  node ",common," ",Node}]),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_node(HostName,NodeName,CookieStr,PaArgs,EnvArgs,NodeDirBase)->
    {ok,Node}=node:ssh_create(HostName,NodeName,CookieStr,PaArgs,EnvArgs),
    {ok,Cwd}=rpc:call(Node,file,get_cwd,[],5000),
    NodeDir=filename:join(Cwd,NodeDirBase),
    []=rpc:call(Node,os,cmd,["rm -rf "++NodeDir],5000),
    timer:sleep(2000),
    ok=rpc:call(Node,file,make_dir,[NodeDir],5000),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						       {"Ok, created node at host",Node,HostName}]),
    {ok,Node,NodeDir}.
    

%%----------------------------------- EOF --------------------------------%%
