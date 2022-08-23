PROJECT_DIR = $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	cabal v2-build $@

.PHONY: deps
deps:
	cabal v2-update
	cabal v2-build --only-dependencies --enable-tests

.PHONY: install
install:
	cabal v2-install --overwrite-policy=always

.PHONY: check test
check test: export GNUPGHOME = $(PROJECT_DIR)/example/gnupg
check test:
	@echo GNUPGHOME=$(GNUPGHOME)
	cabal v2-test

.PHONY: lint
lint:
	hlint executables src tests

modules.png: FORCE
	find executables src -name '*.hs' | xargs graphmod -q | dot -Tpng -o $@

depends.png: hecate.cabal
	cabal-plan dot --tred | dot -Tpng -o $@

.PHONY: nix
nix:
	nix-build release.nix -A hecate

.PHONY: clean
clean:
	cabal v2-clean
	rm -f modules.png depends.png

FORCE:
