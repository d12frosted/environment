.PHONY: clean
clean:
	eldev clean all

.PHONY: bootstrap
bootstrap:
	eldev clean autoloads
	eldev -C --unstable -a -dtT build :autoloads

.PHONY: upgrade
upgrade:
	eldev -C --unstable -a -dtT upgrade

.PHONY: compile
compile:
	eldev clean elc
	eldev -C --unstable -a -dtT compile

.PHONY: lint
lint:
	eldev -C --unstable -a -dtT lint

.PHONY: test
test:
	eldev exec t
	eldev -C --unstable -a -dtT test

.PHONY: vulpea
vulpea:
	eldev -C --unstable exec "(progn (run-hooks 'after-init-hook) (vulpea-db-build))"
