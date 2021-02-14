.PHONY: clean
clean:
	eldev clean all

.PHONY: bootstrap
bootstrap:
	eldev -C --unstable -a -dtT bootstrap

.PHONY: compile
compile:
	eldev clean elc
	eldev -C --unstable -a -dtT build :autoloads
	eldev -C --unstable -a -dtT compile

.PHONY: lint
lint:
	eldev -C --unstable -a -dtT lint

.PHONY: test
test:
	eldev -C --unstable -a -dtT test
