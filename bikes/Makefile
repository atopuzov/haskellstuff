

rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))
SOURCES = $(call rwildcard, , *.hs)
PROJECT_CABAL=bikes.cabal

.PHONY: all
all:
	@echo "Sources:" $(SOURCES)

.PHONY: build
build: ${PROJECT_CABAL}
	cabal new-build

.PHONY: run
run: mqtt.cabal
	cabal new-run

.PHONY: ghci
ghci: ${PROJECT_CABAL}
	cabal new-repl

bikes.cabal: package.yaml
	hpack

bikes.nix: ${PROJECT_CABAL}
	cabal2nix . > bikes.nix

result: $(SOURCES) bikes.nix default.nix
	nix-build

.PHONY: deploy
deploy: RESULT=$(shell readlink result)
deploy: result
	@nix-copy-closure --to root@uk -s $(RESULT)
	@ssh root@uk -- nix-env -i $(RESULT)
