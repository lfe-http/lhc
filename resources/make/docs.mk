.PHONY: docs
REPO = $(shell git config --get remote.origin.url)
DOCS_DIR = docs
DOCS_BUILD_DIR = $(DOCS_DIR)/build
SLATE_GIT_HACK = $(DOCS_DIR)/.git

$(SLATE_GIT_HACK):
	cd $(DOCS_DIR) && ln -s ../.git .

docs-setup:
	cd docs && bundle install

devdocs: $(SLATE_GIT_HACK)
	cd docs && bundle exec middleman server

docs: $(SLATE_GIT_HACK)
	cd docs && rake build

publish: docs
	rm -rf $(DOCS_BUILD_DIR)/.git
	cd $(DOCS_BUILD_DIR) && \
	git init && \
	git add * &> /dev/null && \
	git commit -a -m "Generated content." &> /dev/null && \
	git push -f $(REPO) master:gh-pages && \
	cd - && \
	cd $(DOCS_DIR) && \
	rm .git
