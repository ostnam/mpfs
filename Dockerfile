FROM fpco/stack-build:lts-22.22 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

RUN apt-get update && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

FROM fpco/stack-build:lts-22.22 as back
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build
WORKDIR /opt/build

RUN stack build --system-ghc
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# build frontend
FROM ubuntu:jammy as front
WORKDIR /opt/front
RUN apt update && apt install -y curl
RUN mkdir -p /opt/front
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
COPY elm-src ./elm-src
COPY elm.json .
RUN ./elm make elm-src/Feed.elm --output templates/feeds.html --optimize

# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:jammy as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt update && apt install -y ca-certificates

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=front /opt/front/templates templates
COPY --from=back /opt/build/bin .
COPY static static
CMD ["/opt/app/mpfs-exe", "+RTS", "-M100m", "-RTS"]
