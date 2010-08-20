## Etorrent Makefile
## Try to keep it so simple it can be run with BSD-make as well as
## GNU-make

-include Makefile.config

VER=1.0

SHELL=/bin/sh
ETORRENT_LIB=.
ETORRENT_DEPS=-pa ./deps/gproc/ebin
all: rebar

rebar:
	rebar compile

etorrent:
	$(ERL) -make

tags:
	cd src && $(MAKE) tags

dialyzer: etorrent
	$(DIALYZER) $(DIALYZER_OPTS) --verbose -I $(ETORRENT_LIB)/include -r $(ETORRENT_LIB)/ebin

dialyzer-succ: etorrent
	$(DIALYZER) --verbose --succ_typings -I $(ETORRENT_LIB)/include -r $(ETORRENT_LIB)

run: rebar
	erl -boot start_sasl $(ERL_FLAGS) $(ETORRENT_DEPS) -pa $(ETORRENT_LIB)/ebin \
	-config $(ETORRENT_LIB)/priv/etorrent.config \
	-sname etorrent -s etorrent_app start

tracer:
	erl -boot start_sasl -pa $(ETORRENT_LIB)/ebin -noinput \
	-sname tracer -s tr client

clean:
	rebar clean

