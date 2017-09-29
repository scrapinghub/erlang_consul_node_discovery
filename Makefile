PROJECT = erlang_consul_node_discovery
PROJECT_DESCRIPTION = Plugin which allows to fetch node info from Mesos Consul
PROJECT_VERSION = 0.1.0

NODENAME ?= discovery@127.0.0.1
ERLC_OPTS = +'{parse_transform, rewrite_logging}' -pa ebin


# Whitespace to be used when creating files from templates.
SP = 4

SHELL_OPTS = -name $(NODENAME) -boot start_sasl -s erlang_consul_node_discovery_app start -config config/devel.config

BUILD_DEPS = rewrite_logging
dep_rewrite_logging = git https://github.com/dmzmk/rewrite_logging 0.1.0

# Production deps
DEPS = jiffy
dep_jiffy = git git://github.com/davisp/jiffy.git 0.14.11

# Dev deps
TEST_DEPS = meck
dep_meck  = git git://github.com/eproxus/meck 0.8.7

EUNIT_OPTS = verbose

# Need to make sure that `shell` is not the first target in Makefile
compile: all

devel: all shell

test: eunit ct

include erlang.mk
