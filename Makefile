REBAR=$(shell which rebar || echo ./rebar)

all: compile

compile:
	@if test -d deps; then \
		$(REBAR) update-deps; \
	else \
		$(REBAR) get-deps; \
	fi
	@$(REBAR) compile

clean:
	@$(REBAR) clean
