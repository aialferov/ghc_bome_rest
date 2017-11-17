PROJECT = ghc_bome_rest

REBAR = ./rebar3

BUILD_DIR = _build
TEST_DIR = $(shell $(REBAR) path --app $(PROJECT) --ebin)

CT_LOG_INDEX = _build/test/logs/index.html

all:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

at: all
	$(REBAR) ct --dir $(TEST_DIR)

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

shell:
	$(REBAR) shell
	$(REBAR) unlock

ifeq ($(shell uname), Darwin)
    CT_DISPLAY_CMD = open
endif
ifeq ($(shell uname), Linux)
    CT_DISPLAY_CMD = xdg-open
endif

at-display:
	$(CT_DISPLAY_CMD) $(CT_LOG_INDEX)
