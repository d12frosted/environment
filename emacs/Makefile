.PHONY: all
all: compile lint test

.PHONY: clean
clean:
	eldev clean all

.PHONY: compile
compile:
	eldev -C --unstable -a -dtT build :autoloads
	eldev -C --unstable -a -dtT compile

.PHONY: lint
lint:
	eldev -C --unstable -a -dtT lint

.PHONY: test
test:
	eldev -C --unstable -a -dtT test
