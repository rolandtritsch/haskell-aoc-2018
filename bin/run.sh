#! /bin/bash

if [ "${LANG}" = "HASKELL" ]
then
  if [ "${TYPE}" = "EXE" ]
  then
    target=haskell-aoc-day${DAY}p${PART}
    stack build :${target} --exec ${target}
  else
    target=haskell-aoc-test-day${DAY}
    stack test :${target} --test-arguments "--match Part${PART}"
  fi
else
  ETA_JAVA_ARGS="-Xss1024M -Xmx4096M"
  eta_version=$(etlas exec eta -- --version | cut -d, -f2 | cut -d\  -f3 | sed s/b/./g)

  if [ "${TYPE}" = "EXE" ]
  then
    target=haskell-aoc-day${DAY}p${PART}
    etlas build ${target} && etlas run ${target}
  else
    target=haskell-aoc-test-day${DAY}
    etlas build ${target} && etlas run ${target} -- --match "Part${PART}"
  fi
fi
