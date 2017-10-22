#!/bin/bash

stack exec -- hgettext -o po/batchd.pot -k __ -k __f -k __s -k __sf src/**/*.hs

cd po/
for PO in *.po
do msgmerge -U $PO batchd.pot
done

