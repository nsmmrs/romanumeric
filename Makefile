.DEFAULT_GOAL := installable

.PHONY: installable
installable:
	opam exec -- dune build --root . @install

.PHONY: install
install: installable ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: dev
dev: ## Install development dependencies
	opam install -y --working-dir --deps-only --with-test --with-doc .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: test
test: ## Rebuild and run test suite on every change
	opam exec -- dune build --root . @runtest --force --watch

.PHONY: repl
repl: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: format
format: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: docs
docs: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .
