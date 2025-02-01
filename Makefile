.PHONY: check test
check:
	rebar3 fmt -w
	rebar3 xref
	rebar3 compile
	elp eqwalize-all | tee /dev/stderr | grep -q 'NO ERRORS'
	rebar3 eunit

TAG := $(shell git rev-parse --short HEAD)$(shell git diff --quiet && git diff --cached --quiet || echo "-dirty")
REGION := asia-east2
PROJECT := lucky-antler-449601-f2

build-image:
	@echo "Building Docker image with tag: $(TAG)"
	docker buildx build -o type=docker --target runner --tag yjs_in_erlang_websocket:$(TAG) .
	@echo "IMAGE_TAG=$(TAG)" > .env  # Save latest tag for Docker Compose

# Push the image to Github Container Registry
push-image: build-image
	docker tag yjs_in_erlang_websocket:$(TAG) $(REGION)-docker.pkg.dev/$(PROJECT)/yjs-in-erlang/yjs_in_erlang_websocket:$(TAG)
	docker push $(REGION)-docker.pkg.dev/$(PROJECT)/yjs-in-erlang/yjs_in_erlang_websocket:$(TAG)

up: build-image
	docker-compose up --force-recreate

test:
	rebar3 eunit