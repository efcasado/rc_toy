##==============================================================================
## File: Makefile
##
## GNU make script used to build the project.
##
##
## Author: Enrique Fernandez <enrique.fernandez@erlang-solutions.com>
##==============================================================================

.PHONY: deps rel


## ===========
##  Variables
## ===========

REBAR    := $(shell which rebar)
DEPS_DIR := deps
REL_DIR  := rel


## =======
##  Rules
## =======

all: compile

deps:
	$(REBAR) get-deps

# The 'warnings_as_errors' option is removed from all dependencies' rebar.config
# files to avoid problems when building the project using Erlang/OTP 17
compile: deps
	$(REBAR) compile

rel: compile
	cd $(REL_DIR); $(REBAR) generate

devrel: compile
	@rm -rf devrel/rc_toy1/rc_toy1; cd devrel/rc_toy1; $(REBAR) generate
	@rm -rf devrel/rc_toy2/rc_toy2; cd devrel/rc_toy2; $(REBAR) generate
	@rm -rf devrel/rc_toy3/rc_toy3; cd devrel/rc_toy3; $(REBAR) generate

dev-start:
	@devrel/rc_toy1/rc_toy1/bin/rc_toy1 start
	@devrel/rc_toy2/rc_toy2/bin/rc_toy2 start
	@devrel/rc_toy3/rc_toy3/bin/rc_toy3 start

dev-stop:
	-@devrel/rc_toy1/rc_toy1/bin/rc_toy1 stop
	-@devrel/rc_toy2/rc_toy2/bin/rc_toy2 stop
	-@devrel/rc_toy3/rc_toy3/bin/rc_toy3 stop


clean-deps:
	@rm -rf deps
