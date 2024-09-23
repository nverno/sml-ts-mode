SHELL    = /bin/bash
TSDIR   ?= $(CURDIR)/tree-sitter-sml
TESTDIR ?= $(CURDIR)/test
BINDIR  ?= $(CURDIR)/bin

all:
	@

dev: $(TSDIR)
$(TSDIR):
	@git clone https://github.com/MatthewFluet/tree-sitter-sml
	@cd $(TSDIR) && git checkout main &&                   \
		npm install --progress=true --loglevel=info && \
		npm run build

.PHONY: parse-%
parse-%: dev
	@cd $(TSDIR) && npx tree-sitter parse $(TESTDIR)/$(subst parse-,,$@)

clean:
	$(RM) -r *~

distclean: clean
	$(RM) -rf $$(git ls-files --others --ignored --exclude-standard)
