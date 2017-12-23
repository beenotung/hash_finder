PROJECT = hash_finder
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = erlib hex worker_pool
dep_erlib = git https://github.com/beenotung/erlib.git v0.2.0
dep_hex = git https://github.com/beenotung/hex.git v0.1.0
deps_worker_pool_commit = 3.1.0

LOCAL_DEPS = crypto debugger wx

include erlang.mk

test:
	RELX_CONFIG=$(CURDIR)/testx.config make run
