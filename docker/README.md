batchd docker container
=======================

Build the image with:

    docker build -t batchd .

Run the container with:

    docker run --name batchd -p 9681:9681 -v /path/to/batchd_configs:/etc/batchd batchd

To create superuser, run

    docker exec -it batchd /usr/sbin/batchd-admin

Web client will be accessible on http://localhost:9681.

To run command-line client, use

    docker exec -it batchd batch

