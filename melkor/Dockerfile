FROM haskell:8
MAINTAINER Boris Buliga <boris@d12frosted.io>

COPY ./stack.yaml /opt/server/stack.yaml
COPY ./package.yaml /opt/server/package.yaml

WORKDIR /opt/server

RUN stack setup
RUN stack build --only-dependencies

COPY ./src /opt/server/src
COPY ./app /opt/server/app

RUN stack build
CMD stack exec -- melkor
