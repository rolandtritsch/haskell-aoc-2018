#! /bin/bash

if [ "${TYPE}" = "EXE" ]
then
  target=haskell-aoc-day${DAY}p${PART}
  stack build :${target} --exec ${target}
else
  target=haskell-aoc-test-day${DAY}
  stack test :${target} --test-arguments "--match Part${PART}"
fi
