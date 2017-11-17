PROJECT = ghc_bome_rest

REBAR = ./rebar3

BUILDDIR = _build
TESTDIR = $(shell $(REBAR) path --app $(PROJECT) --ebin)

CTLOGINDEX = _build/test/logs/index.html 

all:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

at: all
	$(REBAR) ct --dir $(TESTDIR)

at-show:
	open $(CTLOGINDEX)

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILDDIR)

shell:
	$(REBAR) shell
	$(REBAR) unlock
