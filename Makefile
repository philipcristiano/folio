PROJECT = folio
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = elvis_mk \
			 erlfmt
DEPS = cowboy \
	   erlsha2 \
	   erlydtl \
	   hackney \
	   jsx

TEST_DEPS = meck
CT_OPTS ?= -create_priv_dir auto_per_tc


dep_cowboy_commit = 2.9.0
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git v0.8.0
dep_erlsha2 = git https://github.com/vinoski/erlsha2.git 2.2.1
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git 0.14.0
dep_hackney = git https://github.com/benoitc/hackney.git 1.18.1
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.10.0

DEP_PLUGINS = elvis_mk

SHELL_OPTS = -eval 'application:ensure_all_started(folio).' -config sys
#SHELL_OPTS = -eval 'application:ensure_all_started(folio).' -config sys
priv/static:
	mkdir -p priv/static

erlfmt:
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) -eval 'erlfmt_cli:do("erlfmt", [write, {files, ["src/*.erl"]} ]), halt(0)'

erlfmt_check:
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) -eval 'erlfmt_cli:do("erlfmt", [check, {files, ["src/*.erl"]} ]), halt(0)'

.PHONY:test
test: tests

include erlang.mk
