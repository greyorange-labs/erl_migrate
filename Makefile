PROJECT = erl_migrate
PROJECT_DESCRIPTION = A tool to upgrade/downgrade schema and migrate data of erlang app's database
PROJECT_VERSION = 0.0.1

DEPS = uuid erlydtl
TEST_DEPS = eunit_formatters meck

dep_uuid = git https://github.com/avtobiff/erlang-uuid v0.5.1
dep_erlydtl = git https://github.com/erlydtl/erlydtl 032746b
dep_eunit_formatters = git https://github.com/seancribbs/eunit_formatters v0.5.0
dep_meck = git https://github.com/eproxus/meck 47b0e96505ae8d22973dcd19f4eb36aef7eb2856

EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored]}}

include erlang.mk
