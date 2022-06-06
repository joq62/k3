all:
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam erl_cra*;
	rm -rf  cluster* logs *.pod_dir rebar.lock;
	rm -rf _build test_ebin ebin *_info_specs;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	echo Done

k3:
	 erl -sname $(sname) -setcookie $(cookie)

check:
	rebar3 check

eunit:
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir *_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
	mkdir  application_info_specs;
	cp ../../specifications/application_info_specs/*.spec application_info_specs;
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	mkdir deployment_info_specs;
	cp ../../specifications/deployment_info_specs/*.depl deployment_info_specs;
	mkdir test_ebin;
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa ebin -pa test_ebin -sname $(sname) -run basic_eunit start -setcookie $(cookie) -k3 clusterid $(clusterid) -k3 num_controllers $(num_controllers) -k3 num_workers $(num_workers) -k3 affinity $(affinity) 
