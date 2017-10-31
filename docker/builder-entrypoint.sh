#!/bin/bash
set -e

curl -sSL https://github.com/portnov/batchd/archive/master.zip -o master.zip
unzip master.zip && rm master.zip
cd batchd-master/
stack install --work-dir=./build --allow-different-user

cp /root/.local/bin/{batchd,batch,batchd-admin} /dst/
