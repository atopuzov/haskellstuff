

rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))
SOURCES = $(call rwildcard, , *.hs)

.PHONY: all
all: build;

.PHONY: build
build: mqtt.cabal
	cabal new-build

.PHONY: run
run: mqtt.cabal
	cabal new-run

.PHONY: ghci
ghci: mqtt.cabal
	cabal new-repl

mqtt.cabal: package.yaml
	hpack

mqtt.nix: mqtt.cabal
	cabal2nix . > mqtt.nix

result: $(SOURCES) mqtt.nix default.nix
	nix-build

.PHONY: deploy
deploy: RESULT=$(shell readlink result)
deploy: result
	@nix-copy-closure --to root@uk -s $(RESULT)
	@ssh root@uk -- nix-env -i $(RESULT)
