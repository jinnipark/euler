.SILENT: state stop

###############################################################################
## Make parameters
###############################################################################
APP=euler
node=$(APP)
cookie=thisisaninsecurecookieforeuler_defineyourown
privdir=priv
datadir=$(privdir)/data
asios=10
procs=10000

# Compile source codes only.
compile:
	./rebar compile


# Start the server
start: compile
	mkdir -p $(datadir)
	erl -pa ebin deps/*/ebin +A $(asios) +K true +P $(procs) +W w +swt low +Mummc 99999 \
		-sname $(node) -setcookie $(cookie) -mnesia dir '"$(datadir)/$(node)"' \
		-boot start_sasl -s reloader -s $(APP) -detached -config $(APP)

# Access debug shell of the running server.
debug:
	erl -pa ebin deps/*/ebin -remsh $(node)@`hostname -s` \
	-sname $(node)_debug -setcookie $(cookie)

# Stop the server
stop:
	erl -pa ebin deps/*/ebin -noinput -hidden -setcookie $(cookie) -sname $(node)_control \
		-s $(APP)_control call $(node)@`hostname -s` stop

# Check the server state
state:
	erl -pa ebin deps/*/ebin -noinput -hidden -setcookie $(cookie) -sname $(node)_control \
		-s $(APP)_control call $(node)@`hostname -s` state

# Perform unit tests.
test: compile
	./rebar eunit skip_deps=true

# Clear all the binaries and dependencies.  The runtime remains intact.
clean: delete-deps
	./rebar clean

# Clear the runtime.
reset:
	rm -rf $(datadir)/$(node)

# Generate documents.
doc:
	./rebar doc

deps: get-deps
	./rebar update-deps

get-deps:
	./rebar get-deps

delete-deps:
	./rebar delete-deps
