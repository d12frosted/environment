.PHONY: help
help:
	@./setup.sh --help

.PHONY: install
install:
	./setup.sh install

.PHONY: bootstrap
bootstrap: install

.PHONY: upgrade
upgrade:
	./setup.sh upgrade

.PHONY: compile
compile:
	./setup.sh compile

.PHONY: lint
lint:
	./setup.sh lint

.PHONY: test
test:
	./setup.sh test

.PHONY: clean
clean:
	./setup.sh clean

.PHONY: doctor
doctor:
	./setup.sh doctor

.PHONY: vulpea
vulpea:
	eldev -C --unstable vulpea-db-build

.PHONY: brb
brb:
	eldev -C --unstable brb-sync
