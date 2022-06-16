%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(basic_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=application:start(k3),
  

  
%    create_controllers(),
    io:format(" sd_server:all() ~p~n",[ sd_server:all()]),
    io:format(" sd_server:get(common) ~p~n",[ sd_server:get(node)]),
    
    
   % [rpc:call(N,init,stop,[],1000)||N<-nodes()],
    
   %% test pod_lib
  

%    init:stop(),
    ok.

    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
create_controllers()->

    StartedControllers=k3_server:started_pods(controller),
    ls(StartedControllers,[]).



ls([],R)->
    R;
ls([Controller|T],Acc)-> 
  %    io:format("Controller ~p~n",[Controller]),
    PodNode=proplists:get_value(pod_node,Controller),
  %  io:format("PodNode ~p~n",[PodNode]),
    
    PodDir=proplists:get_value(pod_dir,Controller),
  %  io:format("PodDir ~p~n",[PodDir]),

    CommonR=pod_lib:load_start(PodNode,PodDir,"common","latest"),
    pod_lib:load_start(PodNode,PodDir,"nodelog","latest"),

    ok=file:make_dir(filename:join(PodDir,"logs")),
    LogFile=filename:join([PodDir,"logs","k3.logs"]),
    rpc:call(PodNode,nodelog_server,create,[LogFile],5000),

    RSd=pod_lib:load_start(PodNode,PodDir,"sd_app","latest"),
    RConfig=pod_lib:load_start(PodNode,PodDir,"config_app","latest"),
    
    
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start common ",CommonR}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start sd_app ",RSd}),
    nodelog_server:log(notice,?MODULE_STRING,?LINE,{"Result start config_app ",RConfig}),
    
    R=pod_lib:load_start(PodNode,PodDir,"divi_app","latest"), 
   
    ls(T,[R|Acc]).
	


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_k3()->
    ok=k3_server:appl_start([]),
    Controllers=k3_server:started_pods(controller),
    io:format("Controllers ~p~n",[Controllers]),
    Workers=k3_server:started_pods(worker),
    io:format("Workers ~p~n",[Workers]),
    []=k3_server:failed_pods(controller),
    []=k3_server:failed_pods(worker),
    AllNodes=nodes(),
    io:format("AllNodes ~p~n",[AllNodes]),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
setup()->
  
    % Simulate host
    R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
 
    R.
