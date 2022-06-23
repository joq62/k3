%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% Desired state: one k3 on each host 
%%% state 0: host missing not running  
%%% state 1: host running and k3 running 
%%% state 10: host running and k3 missing first time
%%% state 11: host running and k3 missing second in row
%%% state 12: host running and k3 missing third in a row -> restart k3 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(k3_orchistrate).  
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 
	 desired_state/1,
	 is_k3_alive/1,
	 is_host_alive/1
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
desired_state(DeploymentName)->
    {ok,Hosts}=db_deployments:read(hosts,DeploymentName),
    AllK3Nodes=sd:get(k3),
%    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DEBUG: AllK3Nodes   ",AllK3Nodes}]),
    AllK3Hosts=[rpc:call(Node,net,gethostname,[],5000)||{Node,_}<-AllK3Nodes],
 %   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DEBUG: AllK3Hosts   ",AllK3Hosts}]),

    MissingHosts=[HostName||HostName<-Hosts,
			    false=:=lists:member({ok,HostName},AllK3Hosts)],

  %  rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DEBUG: MissingHosts   ",MissingHosts}]),

    FailedK3HostsNodes=[rpc:call(Node,net,gethostname,[],5000)||{Node,_}<-AllK3Nodes,
								      pong/=rpc:call(Node,k3,ping,[],5000)],
 %   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DEBUG: FailedK3HostsNodes   ",FailedK3HostsNodes}]),

    FailedK3Hosts=[HostName||{ok,HostName}<-FailedK3HostsNodes],
    
    HostsToRestart=lists:append([MissingHosts,FailedK3Hosts]),
 %   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DEBUG: HostsToRestart   ",HostsToRestart}]),

    Reply=case HostsToRestart of
	      []->
	%	  rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
	%				       {" No Nodes to restart HostsToRestart  ",?MODULE," ",HostsToRestart}]),
		  [];
	      [HostName|_]->
		  rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						{" HostsToRestart  ",?MODULE," ",HostsToRestart}]),
		  case k3_remote_host:start_k3(HostName,DeploymentName)	of
		      {ok,Node,NodeDir,HostName}->
			  rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						       {"OK: started   ",Node," ",NodeDir," ",HostName," ",DeploymentName}]),
			  {ok,Node,NodeDir,HostName};
		      Error ->
			  rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
						       {"ERROR: Failed to start Host Node ",HostName," ",DeploymentName}]),
			  {error,[Error]}
		  end
	  end,
    Reply.


is_k3_alive(_K3Node)->
    false.

is_host_alive(_Host)->

    false.


