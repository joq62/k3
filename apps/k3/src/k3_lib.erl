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
%-include("log.hrl").
%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 create_cluster/4,
	 create_vm/5,
	 delete_vm/1,
	 delete_vm/2,
	 git_load/4
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================
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
create_cluster(ClusterName,Cookie,NumControllers,Affinity)->
    %% set cookie 
    true=erlang:set_cookie(node(),Cookie),
    os:cmd("rm -rf "++ClusterName),
    Reply=case file:make_dir(ClusterName) of
	      {error,Reason}->
		  {error,Reason};
	      ok->
		  Unique=integer_to_list(erlang:system_time(microsecond),36),
		  PodName=ClusterName++"_"++"controller"++"_"++Unique,
		  PodDir=filename:join(ClusterName,PodName++".pod"),
		  ApplId="pod",
		  ApplVsn="0.1.0",
		  pod_lib:create_pod(PodName,PodDir,ApplId,ApplVsn)
	  end,
    Reply.
			  
			  
		  
    %% get available hosts
    %% check Affinity 
    %%
    
 %   {ok,HostName}=net:gethostname(),
  %  UniqueString=integer_to_list(erlang:system_time(microsecond),36),
 %   UniqueString="a",
 %   PodId="controller",
 %   NodeName=ClusterName++"_"++PodId++"_"++UniqueString,
  %  PaArgs=" ",
  %  EnvArgs=" ",
%    {ok,Node}=infra_lib:create_vm(HostName,NodeName,atom_to_list(Cookie),PaArgs,EnvArgs),
%    create_pod(ApplId,ApplVsn,Node,ClusterDir),
 %   start_controller(HostName,NodeName,Cookie,PaArgs,EnvArgs).

start_controller(HostName,NodeName,Cookie,PaArgs,EnvArgs)->
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[],1000),
    timer:sleep(1000),
    Ip="localhost",
    Port=22,
    User="joq62",
    Password="festum01",
    Msg="erl -sname "++NodeName++" -setcookie "++atom_to_list(Cookie)++" "++"-detached",
    TimeOut=5*1000,
    ok=my_ssh:ssh_send(Ip,Port,User,Password,Msg,TimeOut),
    timer:sleep(2000),
    pong=net_adm:ping(Node),
    {ok,Node}.

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

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start_appl(ApplId,ApplVsn,Node) ->
    Reply=case net_adm:ping(Node) of
	      pang->
		  nodelog_server:log(warning,?MODULE_STRING,?LINE,
				     {"Node not started/running ",{error,[eexists,Node]}}),
		  {error,[eexists,Node]};
	      pong->
		  case config:application_gitpath(ApplId) of
		       {error,Err}->
			  nodelog_server:log(warning,?MODULE_STRING,?LINE,
					     {"Error when geting gitpath to application ",ApplId,' ', {error,Err}}),
			  {error,Err};
		      {ok,GitPath}->
			  case rpc:call(Node,service,load,[ApplId,ApplVsn,GitPath],20*5000) of
			      {error,Reason}->
				  nodelog_server:log(warning,?MODULE_STRING,?LINE,{"Error when loading service ",ApplId,' ', {error,Reason}}),
				  {error,Reason};
			      ok ->
				  nodelog_server:log(notice,?MODULE_STRING,?LINE,
						     {"Application  succesfully loaded ",ApplId,' ',ApplVsn,' ',Node}),
				  case rpc:call(Node,service,start,[ApplId,ApplVsn],20*5000) of
				      ok->
					  nodelog_server:log(notice,?MODULE_STRING,?LINE,
							     {"Application  succesfully started ",ApplId,' ',ApplVsn,' ',Node}),
					  ok;
				      Error ->
					  nodelog_server:log(notice,?MODULE_STRING,?LINE,
							     {"Error whenstarting application ",ApplId,' ',Error}),
					  Error					      
				  end
			  end	
		  end	  
	  end,
    Reply.

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

    


