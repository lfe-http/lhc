PROJECT = lhc
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LFE = _build/default/lib/lfe/bin/lfe
DOCS_DIR = $(ROOT_DIR)/priv/mdbook
DOCS_BUILD_DIR = $(ROOT_DIR)/docs
LOCAL_DOCS_HOST = localhost
LOCAL_DOCS_PORT = 5099

FINISH=-run init stop -noshell
GET_VERSION = '{ok,[App]}=file:consult("src/$(PROJECT).app.src"), \
	V=proplists:get_value(vsn,element(3,App)), \
	io:format("~p~n",[V])' \
	$(FINISH)

default: build

build:
	@rebar3 compile

.PHONY: test
test:
	@rebar3 as test do compile, lfe ltest -tall

.PHONY: docs
docs: docs-clean
	@echo "\nBuilding docs ...\n"
	@cd $(DOCS_DIR) && mdbook build -d $(DOCS_BUILD_DIR)

docs-clean:
	@echo "\nCleaning build directories ..."
	@rm -rf $(DOCS_BUILD_DIR)

docs-open: docs-clean
	@echo "\nBuilding docs ...\n"
	@cd $(DOCS_DIR) && mdbook build -d $(DOCS_BUILD_DIR) -o

hex-publish:
	@echo "\nPublishing to hex.pm ...\n"
	rm -rf doc
	mkdir doc
	cp priv/html/docs-redirect.html doc/index.html
	rebar3 hex publish
	rm -rf doc
