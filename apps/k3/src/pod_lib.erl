%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(pod_lib).  
    
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
	 create_pod/2,

	 create_pod/4,
	 delete_pod/2,
	 load_start/4
	]).
	 

%% ====================================================================
%% External functions

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_pod(PodName,PodDir)->
    Reply=case file:make_dir(PodDir) of
	      {error, Reason}->
		  {error, Reason};
	      ok ->
		  infra_lib:create_vm(PodName)
	  end,
    Reply.
			  
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_pod(PodName,PodDir,ApplId,ApplVsn)->
    Reply=case file:make_dir(PodDir) of
	   {error, Reason}->
		  {error, Reason};
	      ok ->
		  case infra_lib:create_vm(PodName) of
		      {error, Reason}->
			  {error, Reason};
		      {ok,PodNode}->
			  case load_start(PodNode,PodDir,ApplId,ApplVsn) of
			      {error, Reason}->
				  {error, Reason};
			      {ok,ApplId,ApplVsn,ApplDir}->
				  {ok,PodNode,PodDir,ApplId,ApplVsn,ApplDir}
			  end
		  end
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start(PodNode,PodDir,ApplId,ApplVsn)->
    Result=case config:application_gitpath(ApplId) of
	       {error,Err}->
		   nodelog_server:log(warning,?MODULE_STRING,?LINE,
				      {"Error when geting gitpath to application ",ApplId,' ', {error,Err}}),
		   {error,Err};
	       {ok,GitPath}->
		   case rpc:call(node(),infra_lib,git_load,[ApplId,ApplVsn,GitPath,PodDir],20*5000) of
		       {error,Reason}->
			   nodelog_server:log(warning,?MODULE_STRING,?LINE,
					      {"Error when loading service ",ApplId,' ', {error,Reason}}),
			   {error,Reason};
		       {ok,ApplDir}->
			   ApplEbin=filename:join(ApplDir,"ebin"),
			   case rpc:call(PodNode,code,add_patha,[ApplEbin],5000) of
			       {error,Reason}->
				   {error,Reason};
			       true->
				   nodelog_server:log(notice,?MODULE_STRING,?LINE,
						      {"Application  succesfully loaded ",ApplId,' ',ApplVsn,' ',PodNode}),
				   case rpc:call(PodNode,application,set_env,[[{pod,[{pod_dir,PodDir}]}]],20*5000) of
				       ok->
					   case rpc:call(PodNode,application,start,[list_to_atom(ApplId)],20*5000) of
					       ok->
						   nodelog_server:log(notice,?MODULE_STRING,?LINE,
								      {"Application  succesfully started ",ApplId,' ',ApplVsn,' ',PodNode}),
						   {ok,ApplId,ApplVsn,ApplDir};
					       Error ->
						   nodelog_server:log(notice,?MODULE_STRING,?LINE,
								      {"Error whenstarting application ",ApplId,' ',Error}),
						   Error
					   end;
				       Error ->
					   Error
				   end
			   end	
		   end	  
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	       

delete_pod(PodNode,PodDir)->
    slave:stop(PodNode),
    os:cmd("rm -rf "++PodDir),
    timer:sleep(500),
    ok.

