REBAR = ./rebar3

BUILDDIR = _build
CTLOGINDEX = _build/test/logs/index.html 

all:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

at:
	$(REBAR) ct

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
