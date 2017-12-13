PROJECT = hash_finder
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = erlib hex
dep_erlib = git https://github.com/beenotung/erlib.git v0.1.3
dep_hex = git https://github.com/beenotung/hex.git v0.1.0

LOCAL_DEPS = crypto

include erlang.mk

test:
	RELX_CONFIG=$(CURDIR)/testx.config make run
