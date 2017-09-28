PROJECT = erlang_consul_node_discovery
PROJECT_DESCRIPTION = Plugin which allows to fetch node info from Mesos Consul
PROJECT_VERSION = 0.1.0

NODENAME ?= discovery@127.0.0.1

# Whitespace to be used when creating files from templates.
SP = 4

SHELL_OPTS = -name $(NODENAME) -boot start_sasl -s erlang_consul_node_discovery_app start -config config/devel.config


# Production deps
DEPS = jiffy erlang_node_discovery
dep_jiffy = git git://github.com/davisp/jiffy.git 0.14.11
dep_erlang_node_discovery = git https://bitbucket.org/scrapinghub/erlang-node-discovery.git 0.1.1


# Dev deps
TEST_DEPS = meck
dep_meck  = git git://github.com/eproxus/meck 0.8.7

EUNIT_OPTS = verbose

devel: all shell

test: eunit ct

include erlang.mk
