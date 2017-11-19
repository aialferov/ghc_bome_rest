PROJECT = ghc_bome_rest

REBAR = ./rebar3

SUITES_DIR = $(shell $(REBAR) path --app $(PROJECT) --ebin)
LOG_DIR = _build/logs

BUILD_DIR = _build

all:
	$(REBAR) compile
	$(REBAR) unlock

at: all
	$(REBAR) ct \
		--dir $(SUITES_DIR) \
		--logdir $(LOG_DIR)

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

shell:
	$(REBAR) shell
	$(REBAR) unlock

ifeq ($(shell uname), Darwin)
    AT_DISPLAY_CMD = open
endif
ifeq ($(shell uname), Linux)
    AT_DISPLAY_CMD = xdg-open
endif

at-display:
	$(AT_DISPLAY_CMD) $(LOG_DIR)/index.html
