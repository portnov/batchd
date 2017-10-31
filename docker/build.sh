#!/bin/bash

# Build the Builder image
docker build -t batchd-builder -f Dockerfile.build .

# Working directory for stack
mkdir -p build
# Directory for built binaries
mkdir -p dst

# Run the builder
docker run --rm -it -v $(pwd)/build:/src/batchd-master/build -v $(pwd)/dst:/dst batchd-builder
# Build runnable image with built binaries.
docker build -t batchd -f Dockerfile .
