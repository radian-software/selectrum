VERSION ?=
CMD ?=

SHELL := bash

# The order is important for compilation.
for_compile := selectrum.el
for_checkdoc := selectrum.el
for_checkindent := $(wildcard *.el)

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: lint
lint: compile checkdoc longlines checkindent toc ## Run all the linters

.PHONY: compile
compile: ## Byte-compile
	@for file in $(for_compile); do \
	    echo "[compile] $$file" >&2 ;\
	    emacs -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc: ## Check docstring style
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" >&2 ;\
	    emacs -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines: ## Check for long lines
	@scripts/check-line-length.bash

.PHONY: checkindent
checkindent: ## Ensure that indentation is correct
	@tmpdir="$$(mktemp -d)"; for file in $(for_checkindent); do \
	    echo "[checkindent] $$file" >&2; \
	    emacs -Q --batch \
	        --eval "(setq inhibit-message t)" \
	        --eval "(load (expand-file-name \"selectrum.el\") nil t)" \
	        --eval "(find-file \"$$file\")" \
	        --eval "(indent-region (point-min) (point-max))" \
	        --eval "(write-file \"$$tmpdir/$$file\")"; \
	    (diff <(cat          "$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") \
	          <(cat "$$tmpdir/$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") ) \
	        | grep -F ">" | grep -o "[a-z].*" | grep . && exit 1 || true; \
	done

.PHONY: toc
toc: README.md ## Update table of contents in README
	@echo "[toc] $^"
	@if command -v markdown-toc >/dev/null; then \
	    markdown-toc -i $^ ; \
	else \
	    echo "  --> markdown-toc missing, skipping" ; \
	fi

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" *.elc >&2
	@rm -f *.elc

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=25.3
	@scripts/docker.bash "$(VERSION)" "$(CMD)"
