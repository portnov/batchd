FROM ubuntu
MAINTAINER Ilya V. Portnov <portnov84@rambler.ru>

RUN apt-get update -y && apt-get install -y ghc alex happy cabal-install
RUN apt-get install -y unzip libpq-dev zlib1g-dev c2hs pkg-config libssh2-1-dev

WORKDIR /usr/src/
ADD https://github.com/portnov/batchd/archive/master.zip .
RUN unzip master.zip && rm master.zip
WORKDIR /usr/src/batchd-master/
RUN cabal update && cabal install
RUN cp dist/build/batchd/batchd /usr/bin/ && \
    cp dist/build/batch/batch /usr/bin && \
    cp dist/build/batchd-admin/batchd-admin /usr/sbin
ADD batchd.yaml /etc/batchd/

VOLUME /etc/batchd/
EXPOSE 9681

CMD ["/usr/bin/batchd"]
