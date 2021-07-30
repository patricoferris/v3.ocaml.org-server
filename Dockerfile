FROM ocaml/opam:alpine as build

# Install system dependencies
RUN sudo apk update && sudo apk add --update libev-dev openssl-dev gmp-dev nodejs npm

WORKDIR /home/opam

# Install Opam dependencies
ADD ocamlorg.opam ocamlorg.opam
RUN opam install . --deps-only

# Install NPM dependencies
ADD package.json package.json
RUN npm install

# Build project
COPY --chown=opam:opam . .
RUN opam exec -- dune build

FROM alpine:3.12 as run

RUN apk update && apk add --update libev gmp git

COPY --from=build /home/opam/_build/default/bin/main.exe /bin/server

ENV OCAMLORG_REPO_PATH /var/opam-repository/
ENV OCAMLORG_DEBUG false

RUN chmod -R 755 /var

RUN git clone https://github.com/ocaml/opam-repository /var/opam-repository

# Download site static files
COPY --from=ocurrent/v3.ocaml.org:live /data asset_site/

EXPOSE 8080

ENTRYPOINT /bin/server
