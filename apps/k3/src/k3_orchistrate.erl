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
    AllK3Nodes=sd_server:get(k3),
    AllK3Hosts=[rpc:call(Node,net,gethostname,[],5000)||Node<-AllK3Nodes],
    MissingHosts=[HostName||HostName<-Hosts,
			    false=:=lists:member({ok,HostName},AllK3Hosts)],

    FailedK3HostsNodes=[rpc:call(Node,net,gethostname,[],5000)||Node<-AllK3Nodes,
								      pong/=rpc:call(Node,k3_server,ping,[],5000)],
    FailedK3Hosts=[HostName||{ok,HostName}<-FailedK3HostsNodes],
    
    HostsToRestart=lists:append([MissingHosts,FailedK3Hosts]),
    Reply=case HostsToRestart of
	      []->
		  [];
	      [HostName|_]->
		  rpc:cast(node(),nodelog_server,log,[notice,?MODULE_STRING,?LINE,
						{" HostsToRestart  ",?MODULE," ",HostsToRestart}]),
		  k3_remote_host:start_k3(HostName,DeploymentName)		  
		%  [k3_remote_host:start_k3(HostName,DeploymentName)||HostName<-HostsToRestart]
	  end,
    Reply.


is_k3_alive(_K3Node)->
    
    false.

is_host_alive(_Host)->

    false.


