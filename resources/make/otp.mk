NODENAME=$(shell echo "simple_cache"|sed -e 's/-//g')

dev:
	@echo "Running OTP app in the foreground ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-eval "(simple-cache:start)"

run: dev

dev-named:
	@echo "Running OTP app in the foreground ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname repl@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval "(simple-cache:start)"

run-named: dev-named

prod:
	@echo "Running OTP app in the background ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname ${NODENAME}@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval "(simple-cache:start)" \
	-noshell -detached

daemon: prod

stop:
	@echo "Stopping OTP app ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname controller@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval "(rpc:call '${NODENAME}@${HOST} 'init 'stop '())" \
	-noshell -s erlang halt

list-nodes:
	@echo "Getting list of running OTP nodes ..."
	@echo
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname controller@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval '(io:format "~p~n" `(,(element 2 (net_adm:names))))' \
	-noshell -s erlang halt
