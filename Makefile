.DEFAULT_GOAL = help

export SHELL = /bin/bash
export PATH := .:$(pwd)/bin:/usr/local/bin:/usr/bin:/bin

export DB_USER = edu
export DB_PASSWORD = edu

OPTS ?= --jobs 8
STACK ?= stack ${OPTS}

dev: ## build continuously
dev: clean buildc

build: ## build project
build: clean bin
	${STACK} build

buildc:  ## build continuously
buildc: clean bin
	${STACK} build --file-watch

buildv: ## build verbosely
buildv: clean bin
	${STACK} build --verbose

clean: ## clean
	rm -f bin
	stack clean

clobber: ## remove stack's work dir
clobber: clean
	rm -rf .stack-work/*

OS = $(shell if [ "$$(uname -s)" = "Darwin" ]; then echo -n osx; else echo -n linux; fi)
LTS = $(shell egrep '^resolver:' stack.yaml | awk '{print $$2}')
GHC_VER = $(shell stack ghc -- --version | awk '{print $$NF}')
bin: ## make "bin" link
	ln -fs .stack-work/install/x86_64-${OS}/${LTS}/${GHC_VER}/bin

repl: ## ghci
	stack exec ghci -- -fobject-code

run-%: ## make run-<cmd>, e.g.: make run-ghci
	stack exec $(word 2, $(subst -, ,$@))

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
