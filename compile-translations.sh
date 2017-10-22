#!/bin/bash

NAME=batchd

for PO in po/*.po
do LANG=$(basename $PO .po)
   DIR=mo/$LANG/LC_MESSAGES
   mkdir -p $DIR
   msgfmt $PO -o $DIR/$NAME.mo
done

