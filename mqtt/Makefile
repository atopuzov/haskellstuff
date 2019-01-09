

rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))
SOURCES = $(call rwildcard, , *.hs)

.PHONY: all
all:
	@echo "Mqtt sources:" $(SOURCES)

.PHONY: build
build:
	stack build

.PHONY: run
run:
	stack ghci

.PHONY: ghci
ghci:
	stack ghci

result: $(SOURCES)
	nix-build

.PHONY: deploy
deploy: RESULT=$(shell readlink result)
deploy: result
	@nix-copy-closure --to root@uk -s $(RESULT)
	@ssh root@uk -- nix-env -i $(RESULT)
