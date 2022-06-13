all:
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam erl_cra*;
	rm -rf  cluster* logs *.pod_dir rebar.lock;
	rm -rf _build test_ebin ebin
	rm -rf deployments temp *_info_specs;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	echo Done
eunit:
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments host_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	git clone https://github.com/joq62/deployments.git;
#	Delete and create new cluster dir to make a clean start
	rm -rf $(clusterid);
	mkdir $(clusterid);
#	copy host info and deployments to make cluster independet
	mkdir  $(clusterid)/host_info_specs;
	cp host_info_specs/*  $(clusterid)/host_info_specs;
	mkdir  $(clusterid)/deployments;
	cp deployments/*  $(clusterid)/deployments;
#	clone application and deployment specs into cluster dir
	rm -rf temp;
	mkdir temp;
	git clone https://github.com/joq62/application_info_specs.git temp;
	mkdir $(clusterid)/application_info_specs;
	mv temp/* $(clusterid)/application_info_specs;
	rm -rf temp;
	mkdir temp;
	git clone https://github.com/joq62/deployment_info_specs.git temp;
	mkdir $(clusterid)/deployments_info_specs;
	mv temp/* $(clusterid)/deployments_info_specs;
#	simulate git clone k3
	mkdir $(clusterid)/k3;
	mkdir $(clusterid)/k3/ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* $(clusterid)/k3/ebin;
#	testing
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa $(clusterid)/* -pa $(clusterid)/k3/ebin -pa test_ebin -sname k3 -run basic_eunit start -k3 deployment_file $(deployment_file)
