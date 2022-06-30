PROJECT_DIR = $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

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

.PHONY: check
check: export GNUPGHOME= $(PROJECT_DIR)/example/gnupg
check:
	cabal v2-test

.PHONY: lint
lint:
	hlint executables src tests

.PHONY: graphmod
graphmod:
	find executables src -name '*.hs' | xargs graphmod -q | xdot -

.PHONY: nix
nix:
	nix-build release.nix -A hecate

.PHONY: clean
clean:
	cabal v2-clean
