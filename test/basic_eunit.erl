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
    %% test pod_lib
    ok=application:start(k3),

    {state,cluster1,cluster1_cookie,undefined,undefined,undefined,
     [{"c202",'cluster1_k3@c202'},{"c200",'cluster1_k3@c200'},{"c201",'cluster1_k3@c201'},{"c100",'cluster1_k3@c100'}],
     undefined
    }=k3_server:read_state(),
    NumControllers=3,
    NumWorkers=20,

    ok=k3_server:create_cluster(NumControllers,NumWorkers,["c100"]),

    {state,"cluster1",cluster1_cookie,3,[non],undefined,{_Date,_Time}}=k3_server:read_state(),

    {error,[already_started,"cluster1"]}=k3_server:create_cluster("cluster1",cluster1_cookie,3,[non]),
        
    {error,[eexists,"glurk"]}=k3_server:delete_cluster("glurk"),
    
    ok=k3_server:delete_cluster("cluster1"),

    {state,undefined,undefined,undefined,undefined,undefined,undefined}=k3_server:read_state(),
    {error,[eexists,"cluster1"]}=k3_server:delete_cluster("cluster1"),



  %  ok=host_server:delete_pod(PodNode),
  %  {error,[eexists,PodNode]}=host_server:delete_pod(PodNode),
  %  pang=net_adm:ping(PodNode),
  %  false=filelib:is_dir(PodDir),
    

    
  %  pong=service:ping(),
  %  timer:sleep(5000),
 %   io:format("AllApplications ~p~n",[application:which_applications()]),
    
  %  ok=t1_test(),
   % ok=t2_test(),
    
   % ok=t22_test(),
%   ok=test3(),
 %   ok=test4(),
%    init:stop(),
    ok.


t1_test()->
  %  [{{"divi_app","1.0.0"},"https://github.com/joq62/divi_app.git"}]=controller:all_specs(),
    io:format("~n"),
    io:format("***************************************************************~n"),
    io:format("~n"),
    AllSpecs=controller:all_specs(),
    io:format("AllSpecs ~p~n",[AllSpecs]),
    {ok,Vm1}=controller:create_vm(),
   % pong=rpc:call(Vm1,sd,all,[],5000),
    ok=controller:load_start_appl("divi_app","1.0.0",Vm1),
    
    [Vm1]=sd:get_host(divi_app,"c100"),
    []=sd:get_host(divi_app,"c200"),

    
    ok=controller:load_start_appl("sd_app","1.0.0",Vm1),
    [Vm1]=rpc:call(Vm1,sd,get,[divi_app],2000),
    []=rpc:call(Vm1,sd,get_host,[divi_app,node()],2000),
  

    42.0=rpc:call(Vm1,mydivi,divi,[420,10],5000),
    ok=controller:stop_unload_appl("divi_app","1.0.0",Vm1),
    {badrpc,_}=rpc:call(Vm1,mydivi,divi,[420,10],5000),
    pong=rpc:call(Vm1,service,ping,[],5000),
    ok=controller:delete_vm(Vm1),
    {badrpc,nodedown}=rpc:call(Vm1,service,ping,[],5000),
    ok.
txx1_test()->
    
    AllDeploymentInfo=config:all_info(),
    ToDeploy=[{ApplId,ApplVsn}||{DeplId,DeplVsn,ApplId,ApplVsn,[all]}<-AllDeploymentInfo],
    io:format("ToDeploy ~p~n",[ToDeploy]),
    ToLoadStart1=[config:find(ApplId,ApplVsn)||{ApplId,ApplVsn}<-ToDeploy],
    ToLoadStart2=[{AppId,Vsn,GitPath}||{AppId,[Vsn|_],GitPath}<-ToLoadStart1],
       
    {ok,ServiceVm}=lib_vm:create(),
    true=rpc:call(ServiceVm,code,add_patha,["ebin"],5000),
    ok=rpc:call(ServiceVm,application,start,[service_app],5000),
    {badrpc,nodedown}=rpc:call(ServiceVm,service,ping,[],5000),
    
    StartR=[load_start_appl(ApplId,Vsn,Git,ServiceVm)||{ApplId,Vsn,Git}<-ToLoadStart2],
    io:format("StartR ~p~n",[StartR]),
   
    42.0=rpc:call(ServiceVm,mydivi,divi,[420,10],5000),
   % ok=rpc:call(ServiceVm,service,stop,[AppId,Vsn],5000),
   % ok=rpc:call(ServiceVm,service,unload,[AppId,Vsn],5000),
   % shutdown_ok=rpc:call(ServiceVm,service,stop,[],5000),
    
    ok.

load_start_appl(ApplId,Vsn,GitPath,ServiceVm)->
    ok=rpc:call(ServiceVm,service,load,[ApplId,Vsn,GitPath],5000),
    ok=rpc:call(ServiceVm,service,start,[ApplId,Vsn],5000),
    ok.
    

stop_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    service:stop(AppId,Vsn).
unload_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    service:unload(AppId,Vsn).
   
start_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    service:start(AppId,Vsn).

load_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    GitPath="https://github.com/joq62/divi_app.git",
    service:load(AppId,Vsn,GitPath).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------


server_test()->
     [controller@c200,
     controller@c201,
     controller@c202,
     controller@c203]=lists:sort(config:nodes_to_connect()),

    [{"config_app",["1.0.0"],"https://github.com/joq62/config_app.git"},
     {"rb_my_divi",["1.0.0"],"https://github.com/joq62/rb_my_divi.git"}]=lists:sort(config:all()),
    
    true=config:member("config_app"),
    true=config:member("config_app","1.0.0"),
    false=config:member("config_app","1.0.1"),
    
    false=config:member("glurk"),
    false=config:member("glurk","1.0.0"),

    {"rb_my_divi",["1.0.0"],"https://github.com/joq62/rb_my_divi.git"}=config:find("rb_my_divi"),
    {"rb_my_divi",["1.0.0"],"https://github.com/joq62/rb_my_divi.git"}=config:find("rb_my_divi","1.0.0"),
    false=config:find("rb_my_divi","1.2.0"),
    
    false=config:find("glurk"),
    false=config:find("glurk","1.0.0"),
      
    ["deployment_specs/conbee.depl",
    "deployment_specs/controller.depl"]=lists:sort(config:all_files()),
    [{"conbee","1.0.0","conbee_app","1.0.0",[controller@c202]},
     {"controller","1.0.0","controller_app","1.0.0",[all]}]=lists:sort(config:all_info()),

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
test4()->
    AllInfo=parse:read_all(),
    gl=io:format("~p~n",[AllInfo]),
    Error=parse:error(AllInfo,7),
    gl=io:format("~p~n",[Error]),
    print(Error),
    ok.

print([])->
    ok;
print([Info|T]) ->
    ?debugFmt("~p~n", [Info]),
   % io:format("~p~n",[Info]),
    print(T).
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
