.PHONY: run build exec

build:
	cabal build

exec:
	env $$(cat .env | xargs) cabal run ingestion-service

run: build exec
