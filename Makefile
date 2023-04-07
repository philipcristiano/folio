.PHONY: deps
deps:
	./rebar3 deps

.PHONY: compile
compile:
	./rebar3 compile

.PHONY: shell
shell: compile
	./rebar3 shell

.PHONY: dialyze
dialyze:
	./rebar3 dialyzer

.PHONY: live_ui
live_ui:
	cd ui; npm run dev

.PHONY: test
test:
	rm -rf _build/test/cover
	./rebar3 eunit

.PHONY: rel
rel:
	./rebar3 release

.PHONY: tar
tar:
	./rebar3 tar

format: erlfmt jsfmt
check: erlfmt_check jsfmt_check

erlfmt:
	./rebar3 fmt -w

erlfmt_check:
	./rebar3 fmt --check

jsfmt:
	cd ui; npx prettier --write .

jsfmt_check:
	cd ui; npx prettier -c .

.PHONY: services
services:
	foreman start -f Procfile.dev
