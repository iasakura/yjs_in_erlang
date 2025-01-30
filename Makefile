.PHONY: check test
check:
	rebar3 fmt -w
	rebar3 xref
	rebar3 compile
	elp eqwalize-all | tee /dev/stderr | grep -q 'NO ERRORS'
	rebar3 eunit

TAG := $(shell git rev-parse --short HEAD)$(shell git diff --quiet && git diff --cached --quiet || echo "-dirty")

build-image:
	@echo "Building Docker image with tag: $(TAG)"
	docker buildx build -o type=docker --target runner --tag yjs_in_erlang_websocket:$(TAG) .
	@echo "IMAGE_TAG=$(TAG)" > .env  # Save latest tag for Docker Compose

up: build-image
	docker-compose up --force-recreate

test:
	rebar3 eunit