

all:
	@echo "mqtt"

build:
	stack build

run:
	stack ghci

ghci:
	stack ghci

result:
	nix-build

deploy: RESULT=$(shell readlink result)
deploy: result
	@nix-copy-closure --to root@uk -s $(RESULT)
	@ssh root@uk -- nix-env -i $(RESULT)
