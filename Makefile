PROJECT_DIR = $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	cabal new-build $@

.PHONY: install
install:
	cabal new-install --overwrite-policy=always

.PHONY: check
check: export GNUPGHOME= $(PROJECT_DIR)/example/gnupg
check:
	cabal new-test

.PHONY: lint
lint:
	cabal new-test test:hlint

.PHONY: graphmod
graphmod:
	find executables src -name '*.hs' | xargs graphmod -q | xdot -

.PHONY: nix
nix:
	nix-build release.nix -A hecate
