#!/bin/bash

export TYPE=EXE

for d in {1..2}
do
  export DAY=0${d}
  for p in {1..2}
  do
    export PART=${p}
    export LANG=HASKELL && ./bin/run.sh
    export LANG=ETA && ./bin/run.sh
  done
done
