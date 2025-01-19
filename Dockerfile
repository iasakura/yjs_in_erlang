FROM erlang:26.2.5.6 AS builder

WORKDIR /app/src
ENV REBAR_BASE_DIR=/app/_build

COPY rebar.config rebar.lock ./
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder AS prod_compiled

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod compile

FROM prod_compiled AS releaser

WORKDIR /app/src

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# build the release tarball and then unpack
# to be copied into the image built in the next stage
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM debian:bookworm-slim AS runner

WORKDIR /opt/yjs_in_erlang

ENV COOKIE=yjs_in_erlang \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp

RUN rm -f /etc/apt/apt.conf.d/docker-clean

# openssl needed by the crypto app
RUN --mount=target=/var/lib/apt/lists,id=apt-lists,type=cache,sharing=locked \
    --mount=type=cache,id=apt,sharing=locked,target=/var/cache/apt \
    apt update && apt install --no-install-recommends -y openssl ncurses-bin

COPY --from=releaser /opt/rel .

ENTRYPOINT ["/opt/yjs_in_erlang/bin/yjs_in_erlang"]
CMD ["foreground"]
