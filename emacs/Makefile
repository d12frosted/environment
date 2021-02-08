.PHONY: all
all: compile lint test

.PHONY: clean
clean:
	eldev clean all

.PHONY: compile
compile:
	eldev -C --unstable -p -dtT compile

.PHONY: lint
lint:
	eldev -C --unstable -p -dtT lint

.PHONY: test
test:
	eldev -C --unstable -p -dtT test
