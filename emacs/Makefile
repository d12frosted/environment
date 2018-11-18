NUCLEUS = "nucleus/bin/nucleus"
MODULES = $(patsubst modules/%/, %, $(sort $(dir $(wildcard modules/*/ modules/*/*/))))

all:
	@$(NUCLEUS) refresh

## Shortcuts
a: autoloads
i: install
u: update
U: upgrade
r: autoremove
c: compile
cc: compile-core
cp: compile-plugins
re: recompile
d: doctor

quickstart:
	@$(NUCLEUS) quickstart

## Package management
install:
	@$(NUCLEUS) install
update:
	@$(NUCLEUS) update
autoremove:
	@$(NUCLEUS) autoremove
autoloads:
	@$(NUCLEUS) autoloads
upgrade:
	@$(NUCLEUS) upgrade

## Byte compilation
compile:
	@$(NUCLEUS) compile
compile-core:
	@$(NUCLEUS) compile :core
compile-private:
	@$(NUCLEUS) compile :private
compile-plugins:
	@$(NUCLEUS) compile :plugins
recompile:
	@$(NUCLEUS) recompile
clean:
	@$(NUCLEUS) clean
# compile-module
# compile-module/submodule
$(patsubst %, compile-%, $(MODULES)): | .local/autoloads.el
	@$(NUCLEUS) $@ $(subst compile-, , $@)

## Unit tests
test:
	@$(NUCLEUS) test
test-core:
	@$(NUCLEUS) test :core
# test-module
# test-module/submodule
$(patsubst %, test-%, $(MODULES)):
	@$(NUCLEUS) test $(subst test-, , $@)

## Utility tasks
# Runs Emacs from a different folder than ~/.emacs.d; only use this for testing!
run:
	@$(NUCLEUS) run $(ARGS)
# Prints debug info about your current setup
info:
	@$(NUCLEUS) info

# Diagnoses potential OS/environment issues
doctor:
	@$(NUCLEUS) doctor

.PHONY: all compile test testi clean
