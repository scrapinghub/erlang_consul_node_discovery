PROJECT = erlang_consul_node_discovery
PROJECT_DESCRIPTION = Plugin which allows to fetch node info from Mesos Consul
PROJECT_VERSION = 0.1.0

NODENAME ?= discovery@127.0.0.1

# Whitespace to be used when creating files from templates.
SP = 4

SHELL_OPTS = -name $(NODENAME) -boot start_sasl -s erlang_consul_node_discovery_app start -config config/devel.config

DEP_PLUGINS = elvis_mk
BUILD_DEPS = elvis_mk
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

# Production deps
DEPS = jiffy
dep_jiffy = git git://github.com/davisp/jiffy.git 1.1.1

# Dev deps
TEST_DEPS = meck
dep_meck  = git git://github.com/eproxus/meck 0.9.2

EUNIT_OPTS = verbose

# Need to make sure that `shell` is not the first target in Makefile
compile: all

devel: all shell

test: eunit ct

PLT_APPS = inets
DIALYZER_OPTS = -Werror_handling -Wrace_conditions -Wno_match
include erlang.mk
