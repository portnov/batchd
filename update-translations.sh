#!/bin/bash

HS=$(find src/ batchd-core/src batchd-amazonka/src batchd-docker/src/ batchd-libvirt/src -name \*.hs)
stack exec -- hgettext -o po/batchd.pot -k __ -k __f -k __s -k __sf $HS

cd po/
for PO in *.po
do msgmerge -U $PO batchd.pot
done

