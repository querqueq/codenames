all: server client

server: server-depends
	stack build --stack-yaml=server/stack.yaml

.PHONY: client lib server
