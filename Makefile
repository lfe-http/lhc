PROJECT = lhc
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LFE = _build/$(REBAR_PROFILE)/lib/lfe/bin/lfe
REBAR3 = PATH=.:$(PATH) rebar3

include priv/make/docs.mk

default: build

build:
	@rebar3 compile

.PHONY: test
test:
	@rebar3 as test do compile, lfe ltest -tall

hex-publish:
	@echo "\nPublishing to hex.pm ...\n"
	rm -rf doc
	mkdir doc
	cp priv/html/docs-redirect.html doc/index.html
	rebar3 hex publish
	rm -rf doc
