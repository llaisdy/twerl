all: compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

dev: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -s reloader start

eunit: compile
	./rebar eunit app=twerl

espec: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -eval 'espec:run([stream_client_spec,stream_client_util_spec]), halt().'

clean:
	./rebar clean
	rm -Rf .eunit
