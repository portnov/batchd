FROM ubuntu
MAINTAINER Ilya V. Portnov <portnov84@rambler.ru>

RUN apt-get update -y && apt-get install -y libpq5 libffi6 libssh2-1 libvirt0

RUN mkdir -p /opt/batchd/{bin,sbin}
ADD dst/batchd /opt/batchd/bin/batchd
ADD dst/batch /opt/batchd/bin/batch
ADD dst/batchd-admin /opt/batchd/sbin/batchd-admin

EXPOSE 9681

CMD ["/opt/batchd/bin/batchd"]
