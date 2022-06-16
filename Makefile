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
	rm -rf _build logs log;
	rm -rf rebar.lock;
	rm -rf ebin;
	rebar3 compile;
	mkdir ebin;
	cp _build/default/lib/*/ebin/* ebin;
#	testing
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa ebin -pa test_ebin -sname k3 -run basic_eunit start
