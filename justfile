set dotenv-load

IMAGE_TAG := `git rev-parse --short HEAD` + `git diff --quiet && git diff --cached --quiet || echo "-dirty"`

check:
    rebar3 fmt -w
    rebar3 xref
    rebar3 compile
    elp eqwalize-all | tee /dev/stderr | grep -q 'NO ERRORS'
    rebar3 eunit

build-image:
    @echo "Building Docker image with tag: {{IMAGE_TAG}}"
    docker buildx build -o type=docker --target runner --tag yjs_in_erlang_websocket:{{IMAGE_TAG}} -f docker/Dockerfile .

# Push the image to Github Container Registry
push-image: build-image
    docker tag yjs_in_erlang_websocket:{{IMAGE_TAG}} ${REGION}-docker.pkg.dev/${PROJECT}/yjs-in-erlang/yjs_in_erlang_websocket:{{IMAGE_TAG}} \
    && docker push ${REGION}-docker.pkg.dev/${PROJECT}/yjs-in-erlang/yjs_in_erlang_websocket:{{IMAGE_TAG}}

up: build-image
    export IMAGE_TAG="{{IMAGE_TAG}}" \
    && docker-compose up --force-recreate

test:
    rebar3 eunit