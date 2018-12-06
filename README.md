[![CircleCI](https://circleci.com/gh/rolandtritsch/haskell-aoc-2018.svg?style=svg)](https://circleci.com/gh/rolandtritsch/haskell-aoc-2018) [![Build Status](https://travis-ci.org/rolandtritsch/haskell-aoc-2018.svg?branch=master)](https://travis-ci.org/rolandtritsch/haskell-aoc-2018) [![GitHub issues](https://img.shields.io/github/issues/rolandtritsch/haskell-aoc-2018.svg)](https://github.com/rolandtritsch/haskell-aoc-2018/issues)

# [Advent of Code](https://adventofcode.com) - 2018 (Haskell Edition)

Note: Complete solutions (and a benchmark) for 2017 (in Scala, Haskell and Eta) can be found ([here](https://github.com/rolandtritsch/scala-aoc-2017); star it, if you like it :)).

To make this work you need to ...

* install [stack](https://www.haskellstack.org)
* run `stack test`
* run `stack build`
* run `stack exec haskell-aoc-exe`

Note: The code also compiles with [Eta](https://eta-lang.org). To make this work you need to ...

* install [etlas](https://eta-lang.org/docs/user-guides/eta-user-guide/installation/etlas)
* run `etlas test`
* run `etlas build`
* run `etlas run haskell-aoc-exe`

Note: You need to have stack installed and need to run `stack build --dry-run` first (to generate the `cabal` file (the dry-run will invoke hpack to generate the cabal file from the package.yaml file)).

Locally you can run all solutions with `./bin/run-local.sh | grep -e "->"` (or you can fork the repo into your account and enable a/the travis build).

## Benchmark

The solutions also measure the elapse time in seconds.

With that we can do a very simple [benchmark](https://docs.google.com/spreadsheets/d/1kHugZ-8mJczlmQRcda23YGvAgeqlJLt1I7cYlDD3Tws/edit?usp=sharing).

![Benchmark](https://www.dropbox.com/s/m6sztfj7qkybbdx/benchmark-2018.png?dl=0&raw=1)

To create the numbers you can just run ...

* `stack build haskell-aoc-exe && stack exec haskell-aoc-exe`
* `etlas build haskell-aoc-exe && etlas run haskell-aoc-exe`

Note: To run the eta solutions you need to `export ETA_JAVA_ARGS="-Xss1024M -Xmx4096M"`.
