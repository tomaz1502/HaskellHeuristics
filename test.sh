#!/bin/bash

stack build

INPDIR="./Inputs"

for FILE in $(ls $INPDIR)
do
  printf "Running: %s\n" $FILE
  stack run < $INPDIR/$FILE
done
