REBAR=$(shell which rebar || echo ./rebar)

all: compile

compile: $(REBAR)
	@$(REBAR) get-deps
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean
