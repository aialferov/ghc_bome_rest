REBAR = ./rebar3

BUILDDIR = _build

all:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

at:
	$(REBAR) ct

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILDDIR)

shell:
	$(REBAR) shell
	$(REBAR) unlock
