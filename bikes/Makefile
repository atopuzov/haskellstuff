

rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))
SOURCES = $(call rwildcard, , *.hs)

.PHONY: all
all:
	@echo "Sources:" $(SOURCES)

.PHONY: build
build:
	stack build

.PHONY: run
run:
	stack run

.PHONY: ghci
ghci:
	stack ghci

bikes.nix: package.yaml
	cabal2nix . > bikes.nix

result: $(SOURCES) bikes.nix default.nix
	nix-build

.PHONY: deploy
deploy: RESULT=$(shell readlink result)
deploy: result
	@nix-copy-closure --to root@uk -s $(RESULT)
	@ssh root@uk -- nix-env -i $(RESULT)
