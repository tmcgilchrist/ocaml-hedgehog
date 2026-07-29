.PHONY: docs website-dev website-build

docs:
	GENERATE_MARKDOWN=true dune build @doc @doc-markdown

website-dev: docs
	cd website && npm run dev

website-build: docs
	cd website && npm run build
