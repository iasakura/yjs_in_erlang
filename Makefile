.PHONY: check
check:
	rebar3 fmt -w
	rebar3 xref
	rebar3 compile
	elp eqwalize-all | tee /dev/stderr | grep -q 'NO ERRORS'
	rebar3 eunit

test:
	rebar3 eunit