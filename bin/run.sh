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
  eta_version=$(etlas exec eta -- --version | cut -d, -f2 | cut -d\  -f3 | sed s/b/./g)

  if [ "${TYPE}" = "EXE" ]
  then
    target=haskell-aoc-day${DAY}p${PART}
    etlas build --enable-uberjar-mode ${target}
    java -Xss1024M -Xmx4096M -jar dist/build/eta-${eta_version}/haskell-aoc-0.1.0.0/x/${target}/build/${target}/${target}.jar
else
    target=haskell-aoc-test-day${DAY}
    etlas build --enable-uberjar-mode ${target}
    java -Xss1024M -Xmx4096M -jar dist/build/eta-${eta_version}/haskell-aoc-0.1.0.0/t/${target}/build/${target}/${target}.jar --match "Part${PART}"
  fi
fi
